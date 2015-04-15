-module(aws_http_retry).

-record(retry, {max_nr,
                nr = 0,
                jitter,
                max_backoff}).

-export([init/1, should/3, incr/1, backoff/1]).

init(Opts) ->
    #retry{
       jitter = proplists:get_value(http_retries_jitter, Opts, true),
       max_nr = proplists:get_value(http_retries, Opts, 5),
       max_backoff = proplists:get_value(http_retries_backoff, Opts, 60000)}.

should(#retry{max_nr = Max, nr = Nr}, HttpResp, ResponseCB) ->
    case {Max =< Nr,
          erlang:function_exported(ResponseCB, should_retry, 1)} of
        {true, true}  -> ResponseCB:should_retry(HttpResp);
        {_, _} -> false
    end.

incr(#retry{max_nr = Max} = Retry) -> Retry#retry{max_nr = Max + 1}.

backoff(#retry{max_backoff = Max, nr = Retries, jitter = Jitter}) ->
    BackoffTime = erlang:min(Max, erlang:trunc(math:pow(2, Retries) * 50)),
    if Jitter -> random:uniform(BackoffTime);
       true   -> BackoffTime
    end.
