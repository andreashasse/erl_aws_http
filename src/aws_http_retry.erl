-module(aws_http_retry).

-record(retry, {max_nr,
                nr = 0,
                base_time,
                rand,
                jitter,
                max_backoff}).

-export([init/1, should/3, incr/1, backoff/1]).

init(Opts) ->
    {_, Seed} = random:uniform_s(os:timestamp()),
    #retry{
       jitter = proplists:get_value(http_retries_jitter, Opts, true),
       base_time = proplists:get_value(http_retries_base_time, Opts, 50),
       rand = Seed,
       max_nr = proplists:get_value(http_retries, Opts, 5),
       max_backoff = proplists:get_value(http_retries_backoff, Opts, 60000)}.

should(#retry{max_nr = Max, nr = Nr}, {error, _Rsn}, _ResponseCB) ->
    error_logger:info_msg("~p =< ~p", [Max, Nr]),
    Max >= Nr;
should(#retry{max_nr = Max, nr = Nr}, HttpResp, ResponseCB) ->
    case {Max >= Nr,
          erlang:function_exported(ResponseCB, should_retry, 1)} of
        {true, true}  -> ResponseCB:should_retry(HttpResp);
        {_, _} -> false
    end.

incr(#retry{nr = Nr} = Retry) -> Retry#retry{nr = Nr + 1}.

backoff(#retry{max_backoff = Max, nr = Retries, rand = Rand,
               base_time = BaseTime, jitter = Jitter} = Retry) ->
    BackoffTime =
        erlang:min(Max, erlang:trunc(math:pow(2, Retries) * BaseTime)),
    {RandBackoffTime, NewRandState} = random:uniform_s(BackoffTime, Rand),
    NewRetry = incr(Retry#retry{rand = NewRandState}),
    if Jitter -> {RandBackoffTime, NewRetry};
       true   -> {BackoffTime, NewRetry}
    end.
