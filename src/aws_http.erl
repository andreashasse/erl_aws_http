-module(aws_http).

-include("../src/aws_http.hrl").

-export([post/5, put/5, get/5, delete/4, req/7]).

%% Application api
-ignore_xref([post/5, put/5, get/5, delete/4, req/7]).

%% apply
-export([default_decode/3]).
-ignore_xref([default_decode/3]).

post(Path, Headers, Payload0, Callback, Opts) ->
    Payload = encode_payload(Payload0, Opts),
    req("POST", Path, [], Headers, Payload, Callback, Opts).

put(Path, Headers, Payload0, Callback, Opts) ->
    Payload = encode_payload(Payload0, Opts),
    req("PUT", Path, [], Headers, Payload, Callback, Opts).

get(Path, QueryString, Headers, Callback, Opts) ->
    req("GET", Path, QueryString, Headers, <<"">>, Callback, Opts).

delete(Path,  Headers, Callback, Opts) ->
    req("DELETE", Path, [], Headers, <<"">>, Callback, Opts).

req(Method, Path0, QueryString, Headers, Payload, Callback, Opts) ->
    Retry = aws_http_retry:init(Opts),
    req(Method, Path0, QueryString, Headers, Payload, Callback,
        Retry, Opts).

req(Method, Path0, QueryString, Headers, Payload, Callback, Retry, Opts) ->
    Path = mk_req_path(Path0, QueryString),
    Service = Callback:service_name(),
    Conf = get_conf(Service, Opts),
    URL = Conf#aws_conf.base_url ++ Path,
    NewHeaders = mk_headers(Conf, Method, Path0, QueryString, Headers, Payload),
    HttpResp = do_call(URL, Method, NewHeaders, Payload, Opts),
    case aws_http_retry:should(Retry, HttpResp, Callback) of
        true ->
            timer:sleep(aws_http_retry:backoff(Retry)),
            NewRetry = aws_http_retry:incr(Retry),
            req(Method, Path0, QueryString, Headers, Payload, Callback,
                NewRetry, Opts);
        false ->
            HttpResp
    end.


%% ---------------------------------------------------------------------------
%% Internal

mk_req_path(Path0, QueryString) ->
    Path1 = string:join(["" | Path0], "/"),
    LQueryString = string:join(
                     [K ++ "=" ++ V ||
                         {K, V} <-  QueryString], "&"),
    maybe_add_querystring(Path1, LQueryString).


maybe_add_querystring(Path0, []) -> Path0;
maybe_add_querystring(Path0, Qs) -> Path0 ++ "?" ++ Qs.

do_call(URL, Method, Headers, Payload, Opts) ->
    HttpTimeout = proplists:get_value(http_timeout, Opts, 5000),
    %% FIXME: Handle lhttpcs error messages.
    HttpResp = lhttpc:request(URL, Method, Headers, Payload, HttpTimeout),
    decode_http_resp(HttpResp, Opts).

get_conf(Service, Opts) ->
    ProfileName = proplists:get_value(profile, Opts, default),
    aws_http_cfg:get(ProfileName, Service).

mk_headers(Conf, Method, Path, QueryString, Headers0, Payload) ->
    Headers = Headers0 ++ [{"Host", Conf#aws_conf.endpoint}],
    aws_http_auth_v4:sign_v4(Method, Path, QueryString, Conf, Headers, Payload).

encode_payload(Payload, Opts) ->
    case proplists:get_value(request_body_encode, Opts, {jiffy, encode, []}) of
        none -> Payload;
        {M, F, Args} -> erlang:apply(M, F, [Payload|Args])
    end.

decode_http_resp({ok, {{Code, Msg}, Hdrs, Body}}, Opts) ->
    case proplists:get_value(response_body_decode, Opts,
                             {aws_http, default_decode, []}) of
        none -> {ok, {{Code, Msg}, Hdrs, Body}};
        {M, F, Args} ->
            NewBody = erlang:apply(M, F, [Code, Body,Hdrs|Args]),
            {ok, {{Code, Msg}, Hdrs, NewBody}}
    end.

default_decode(Code, Body, Headers) ->
    case {lists:member(Code, [204, 304]),
          proplists:get_value("Content-Type", Headers)} of
        {false, "application/json"} -> jiffy:decode(Body, [return_maps]);
        {_, _} -> Body
    end.
