-module(aws_http_cfg).

-behaviour(gen_server).

-include("../src/aws_http.hrl").

%% API
-export([start_link/0, get/2]).
%% supervisor spec
-ignore_xref([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(Profilename, Service) ->
    case ets:lookup(?TAB, Profilename) of
        [{Profilename, Conf}] -> maybe_set_endpoint(Service, Conf);
        [] -> throw({missing_profile, Profilename})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?TAB = ets:new(?TAB, [named_table, protected]),
    {ok, Profiles} = application:get_env(aws_http, profiles),
    lists:foreach(fun tab_insert/1, Profiles),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tab_insert({Name, Profile}) ->
    Prop = fun(Key) ->
                   case proplists:get_value(Key, Profile) of
                       undefined -> erlang:error({missing_conf, {Name, Key}});
                       Val -> Val
                   end
           end,
    Key = Prop(aws_access_key_id),
    Secret = Prop(aws_secret_access_key),
    Region = proplists:get_value(region, Profile, "us-east-1"),
    Endpoint = proplists:get_value(endpoint, Profile),
    Conf = #aws_conf{name = Name, key = Key, secret = Secret, region = Region,
                     endpoint = Endpoint},
    ets:insert(?TAB, {Name, Conf}).


maybe_set_endpoint(
  Service, #aws_conf{endpoint = undefined, region = Region} = Conf) ->
    EndPoint = Service ++ "." ++ Region ++ ".amazonaws.com",
    Conf#aws_conf{service = Service,
                  base_url = "https://" ++ EndPoint,
                  endpoint = EndPoint};
maybe_set_endpoint(Service, Conf) -> Conf#aws_conf{service = Service}.
