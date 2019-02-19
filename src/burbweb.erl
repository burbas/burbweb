%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2019, Niclas Axelsson
%%% @doc
%%% Interface module for burbweb
%%% @end
%%% Created : 19 Feb 2019 by Niclas Axelsson <niclas@burbas.se>

-module(burbweb).

-export([
         get_main_app/0,
         application_loaded/1,
         load_routefile/2
        ]).


%%--------------------------------------------------------------------
%% @doc
%% Returns the name of the main bw-application (The one that started
%% everything)
%%
%% @spec get_main_app() -> {ok, Application :: atom()}
%% @end
%%--------------------------------------------------------------------
get_main_app() ->
    burbweb_router:get_main_app().

%%--------------------------------------------------------------------
%% @doc
%% Checks if a burbweb application is loaded
%%
%% @spec application_loaded(Application::atom()) -> true | false
%% @end
%%--------------------------------------------------------------------
application_loaded(Application) ->
    %% First we need to check which application that is the main one
    {ok, MainApp} = application:get_application(burbweb_sup),
    lists:any(fun({CompApp, _, _}) -> CompApp == Application end,
              application:get_env(MainApp, burbweb_applications)).

%%--------------------------------------------------------------------
%% @doc
%% Loads a route-file for a certain application
%%
%% @spec load_routefile(Application :: atom(), route_path :: string()) -> ok
%% @end
%%--------------------------------------------------------------------
load_routefile(Application, RoutePath) ->
    burbweb_router:process_routefile(Application, RoutePath).
