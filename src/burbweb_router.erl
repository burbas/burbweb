%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 24 Jun 2018 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(burbweb_router).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         process_routefile/2,
         add_route/5,
         add_route/6,
         apply_routes/0,
         remove_route/2,
         get_routes/0
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          listener = burbweb_listener :: atom(),
          dispatch_table = [] :: list()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process_routefile(App, Routefile) ->
    gen_server:cast(?SERVER, {process_routes, App, Routefile}).

add_route(App, Module, Func, Route, Security) ->
    add_route(App, Module, Func, '_', Route, Security).

add_route(App, Module, Func, Host, Route, Security) ->
    add_route(App, Module, Func, Host, Route, Security, '_', rest}).

add_route(App, Module, Func, Host, Route, Security, Method, ControllerType) ->
    gen_server:cast(?SERVER, {add_route, App, Module, Func, Host, Route, Security, Method, ControllerType}).

add_static(App, Path, Host, Route) ->
    gen_server:cast(?SERVER, {add_static, App, Path, Host, Route}).

apply_routes() ->
    gen_server:cast(?SERVER, apply_routes).

remove_route(Host, Route) ->
    gen_server:cast(?SERVER, {remove_route, Host, Route}).

get_routes() ->
    gen_server:call(?SERVER, get_routes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Apps =
        case application:get_env(burbweb_applications) of
            undefined -> [];
            {ok, AppsList} -> AppsList
        end,
    lists:foreach(fun load_app_route/1, Apps),
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_routes, _From, State = #state{dispatch_table = DT}) ->
    {reply, {ok, DT}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({process_routes, App, Routefile}, State) ->
    {ok, HostRoutes} = file:consult(Routefile),
    process_routes(App, HostRoutes),
    apply_routes(),
    {noreply, State};



handle_cast({remove_route, Host, Route}, State = #state{dispatch_table = DT}) ->
    case proplists:get_value(Host, DT) of
        undefined ->
            logger:warning("Could not remove route: ~p for host: ~p. Host not found!", [Route, Host]),
            {noreply, State};
        {Host, Routes} ->
            NewRoutes = proplists:delete(Route, Routes),
            case NewRoutes of
                Routes ->
                    logger:warning("Could not remove route: ~p for host: ~p. Route not found!", [Route, Host]),
                    {noreply, State};
                _ ->
                    logger:info("Removed route: ~p from host: ~p", [Route, Host]),
                    DT2 = proplists:delete(Host, DT),
                    NewDT = [{Host, NewRoutes}|DT2],
                    {noreply, State#state{dispatch_table = NewDT}}
            end
    end;

handle_cast(apply_routes, State = #state{dispatch_table = DT,
                                         listener = Listener}) ->
    logger:info("Applying routes. ~p", [DT]),
    Dispatch = cowboy_router:compile(DT),
    cowboy:set_env(Listener, dispatch, Dispatch),
    {noreply, State};

handle_cast({add_static, App, Path, Host, Route}, State = #state{dispatch_table = DT}) ->
    RouteInfo = {Route, cowboy_static, {priv_dir, App, Path}},
    NewDT =
        case proplists:get_value(Host, DT) of
            undefined ->
                [{Host, [RouteInfo]}|DT];
            Routes ->
                [{Host, [RouteInfo|Routes]}|proplists:delete(Host, DT)]
        end,
    {noreply, State#state{dispatch_table = NewDT}};

handle_cast({add_route, App, Module, Func, Host, Route, Secure, Method, ControllerType}, State = #state{dispatch_table = DT}) ->
    InitialState = #{app => App,
		     mod => Module,
		     func => Func,
		     secure => Secure,
		     method => Method,
		     type => ControllerType},
    RouteInfo = {Route, burbweb_controller, InitialState},

    NewDT =
        case proplists:get_value(Host, DT) of
            undefined ->
                [{Host, [RouteInfo]}|DT];
            Routes ->
                [{Host, [RouteInfo|Routes]}|DT]
        end,

    {noreply, State#state{dispatch_table = NewDT}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_app_route(#{name := AppName, routes_file := RouteFile}) ->
    case code:lib_dir(AppName) of
        {error, bad_name} ->
            logger:warning("Could not find the application ~p. Check your config and rerun the application", [AppName]),
            ok;
        Filepath ->
            RouteFilePath = filename:join([Filepath, RouteFile]),
            process_routefile(AppName, RouteFilePath)
    end;
load_app_route(RouteInfo = #{name := AppName}) ->
    Routename = "priv/" ++ erlang:atom_to_list(AppName) ++ ".routes.erl",
    load_app_route(RouteInfo#{routes_file => Routename}).

process_routes(_, []) ->
    ok;
process_routes(App, [RoutesLine|T]) ->
    Prefix = maps:get(prefix, RoutesLine, ""),
    Host = maps:get(host, RoutesLine, '_'),
    Routes = maps:get(routes, RoutesLine, []),
    Statics = maps:get(statics, RoutesLine, []),
    Secure = maps:get(security, RoutesLine, false),
    add_routes(App, Host, Prefix, Secure, Routes),
    add_statics(App, Host, Prefix, Statics),
    process_routes(App, T).

add_routes(_, _, _, _, []) ->
    ok;
add_routes(App, Host, Prefix, Secure, [{Route, Module, Func} | T]) ->
    add_route(App, Module, Func, Host, Prefix ++ Route, Secure),
    add_routes(App, Host, Prefix, Secure, T);
add_routes(App, Host, Prefix, Secure, [{Route, Method, Module, Func, ControllerType} | T]) ->
    add_route(App, Module, Func, Host, Prefix ++ Route, Secure, Method, ControllerType),
    add_routes(App, Host, Prefix, Secure, T).

add_statics(_, _, _, []) ->
    ok;
add_statics(App, Host, Prefix, [{Route, Path} | T]) ->
    add_static(App, Path, Host, Prefix ++ Route),
    add_statics(App, Host, Prefix, T).

get_method(get) ->
    <<"GET">>;
get_method(post) ->
    <<"POST">>;
get_method(delete) ->
    <<"DELETE">>;
get_method(put) ->
    <<"PUT">>;
get_method('_') ->
    '_';
get_method(Other) ->
    logger:error("Unsupported method in routes: ~p", [Other]),
    throw(unsupported_method).
