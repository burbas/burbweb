%%%-------------------------------------------------------------------
%% @doc burbweb public API
%% @end
%%%-------------------------------------------------------------------

-module(burbweb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    burbweb_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_cowboy() ->
    Port = application:get_env(burbweb, port, 8080),
    {ok, _} = cowboy:start_clear(
                burbweb_listener,
                [{port, Port}],
                #{}).
