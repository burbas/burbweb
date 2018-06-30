%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 26 Jun 2018 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(burbweb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% This is a bit ugly, but we need to do this anyhow(?)
    application:ensure_all_started(ranch),
    start_cowboy(),
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Children = [
                child(burbweb_compiler, burbweb_compiler),
                child(burbweb_router, burbweb_router),
                child(burbweb_session, burbweb_session)
               ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Id, Mod) ->
    #{id => Id,
      start => {Mod, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Mod]}.


start_cowboy() ->
    Port =
        case application:get_env(web_port) of
            undefined -> 8080;
            WebPort -> WebPort
        end,
    {ok, _} = cowboy:start_clear(
                burbweb_listener,
                [{port, Port}],
                #{}).
