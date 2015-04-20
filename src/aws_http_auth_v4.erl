%% NOTE: Some of this code is copied from github.com/gleber/erlcloud/blob/master/src/erlcloud_aws.erl

-module(aws_http_auth_v4).

-export([sign_v4/6]).

-include("../src/aws_http.hrl").

authorization(Config, CredentialScope, SignedHeaders, Signature) ->
    ["AWS4-HMAC-SHA256"
     " Credential=", Config#aws_conf.key, $/, CredentialScope, $,,
     " SignedHeaders=", SignedHeaders, $,,
     " Signature=", Signature].

sign_v4(Method, Path, QueryString, Conf, Headers, Payload) ->
    SignPaths = lists:map(fun http_uri:encode/1, Path),
    SignPath = string:join(["" | SignPaths], "/"),
    SignQueryString = string:join(
                        [http_uri:encode(K) ++ "=" ++ http_uri:encode(V) ||
                            {K, V} <-  QueryString], "&"),
    Date = iso_8601_basic_time(),
    Headers1 = [{"x-amz-date", Date} | Headers],
    {Request, SignedHeaders} = canonical_request(
                                 Method, SignPath, SignQueryString,
                                 Headers1, Payload),
    CredentialScope = credential_scope(Date, Conf),
    ToSign = to_sign(Date, CredentialScope, Request),
    SigningKey = signing_key(Conf, Date),
    Signature = base16(sha256_mac(SigningKey, ToSign)),
    Authorization = authorization(Conf, CredentialScope, SignedHeaders, Signature),
    [{"Authorization", lists:flatten(Authorization)} | Headers1].

signing_key(Conf, Date) ->
    %% TODO cache the signing key so we don't have to recompute for every request
    DateOnly = string:left(Date, 8),
    KDate = sha256_mac("AWS4" ++ Conf#aws_conf.secret, DateOnly),
    KRegion = sha256_mac(KDate, Conf#aws_conf.region),
    KService = sha256_mac(KRegion, Conf#aws_conf.service),
    sha256_mac(KService, "aws4_request").

to_sign(Date, CredentialScope, Request) ->
    ["AWS4-HMAC-SHA256\n",
     Date, $\n,
     CredentialScope, $\n,
     hash_encode(Request)].

credential_scope(Date, Conf) ->
    Region = Conf#aws_conf.region,
    Service = Conf#aws_conf.service,
    DateOnly = string:left(Date, 8),
    [DateOnly, $/, Region, $/, Service, "/aws4_request"].

canonical_request(Method, CanonicalURI, CanonicalQueryString, Headers, Payload) ->
    {CanonicalHeaders, SignedHeaders} = canonical_headers(Headers),
    {[Method, $\n,
      CanonicalURI, $\n,
      CanonicalQueryString, $\n,
      CanonicalHeaders, $\n,
      SignedHeaders, $\n,
      hash_encode(Payload)],
     SignedHeaders}.

hash_encode(Data) ->
    Hash = sha256(Data),
    base16(Hash).

base16(Data) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Data)]).

canonical_headers(Headers) ->
    Normalized = [{string:to_lower(Name), trimall(Value)} || {Name, Value} <- Headers],
    Sorted = lists:keysort(1, Normalized),
    Canonical = [[Name, $:, Value, $\n] || {Name, Value} <- Sorted],
    Signed = string:join([Name || {Name, _} <- Sorted], ";"),
    {Canonical, Signed}.

trimall(Value) ->
    %% TODO - remove excess internal whitespace in header values
    re:replace(Value, "(^\\s+)|(\\s+$)", "", [global]).

iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

sha256(V) ->
    crypto:hash(sha256, V).

sha256_mac(K, S) ->
    crypto:hmac(sha256, K, S).
