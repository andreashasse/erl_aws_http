-module(aws_http).

-include("../src/aws_http.hrl").

-export([post/5, get/4, req/6]).

%% Application api
-ignore_xref([post/5, get/4, req/6]).

%% apply
-export([default_decode/2]).
-ignore_xref([default_decode/2]).

post(Path, Headers, Payload0, Service, Opts) ->
    Payload = encode_payload(Payload0, Opts),
    req("POST", Path, Headers, Payload, Service, Opts).

get(Path, Headers, Service, Opts) ->
    req("GET", Path, Headers, <<"">>, Service, Opts).

req(Method, Path0, Headers, Payload, Service, Opts) ->
    Path = string:join(["" | Path0], "/"),
    Conf = get_conf(Service, Opts),
    URL = Conf#aws_conf.base_url ++ Path,
    NewHeaders = mk_headers(Conf, Method, Path, Headers, Payload),
    do_call(URL, Method, NewHeaders, Payload, Opts).

%% ---------------------------------------------------------------------------
%% Internal

do_call(URL, Method, Headers, Payload, Opts) ->
    HttpTimeout = proplists:get_value(http_timeout, Opts, 5000),
    HttpResp = lhttpc:request(URL, Method, Headers, Payload, HttpTimeout),
    decode_http_resp(HttpResp, Opts).

get_conf(Service, Opts) ->
    ProfileName = proplists:get_value(profile, Opts, default),
    aws_http_cfg:get(ProfileName, Service).

mk_headers(Conf, Method, Path, Headers0, Payload) ->
    Headers = Headers0 ++ [{"Host", Conf#aws_conf.endpoint}],
    aws_http_auth_v4:sign_v4(Method, Path, Conf, Headers, Payload).

encode_payload(Payload, Opts) ->
    case proplists:get_value(request_body_encode, Opts, {jiffy, encode, []}) of
        none -> Payload;
        {M, F, Args} -> erlang:apply(M, F, [Payload|Args])
    end.

decode_http_resp({ok, {Code, Hdrs, Body}}, Opts) ->
    case proplists:get_value(response_body_decode, Opts,
                             {aws_http, default_decode, []}) of
        none -> {ok, {Code, Hdrs, Body}};
        {M, F, Args} -> {ok, {Code, Hdrs, erlang:apply(M, F, [Body,Hdrs|Args])}}
    end.

default_decode(Body, Headers) ->
    %% FIXME: Check if content type is json with proper mime type lib
    case proplists:get_value("Content-Type", Headers) of
        "application/json" ->
            jiffy:decode(Body, [return_maps]);
        __ ->
            Body
    end.
