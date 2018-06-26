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
         process_routefile/1,
         add_route/3,
         add_route/4,
         apply_routes/0,
         remove_route/2
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
          listener :: atom(),
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

process_routefile(Routefile) ->
    gen_server:cast(?SERVER, {process_routes, Routefile}).

add_route(Module, Func, Route) ->
    add_route(Module, Func, '_', Route).

add_route(Module, Func, Host, Route) ->
    gen_server:cast(?SERVER, {add_route, Module, Func, Host, Route}).

apply_routes() ->
    gen_server:cast(?SERVER, apply_routes).

remove_route(Host, Route) ->
    gen_server:cast(?SERVER, {remove_route, Host, Route}).

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
handle_cast({process_routes, Routefile}, State) ->
    {ok, Routes} = file:consult(Routefile),
    lists:foreach(
      fun(#{host := Host, routes := Routes}) ->
              [ add_route(Module, Func, Host, Route) || {Route, [Module, Func]} <- Routes ];
         (#{routes := Routes}) ->
              [ add_route(Module, Func, Route) || {Route, [Module, Func]} <- Routes ]
         end, Routes),
    {noreply, State};

handle_cast({remove_route, Host, Route}, State = #state{dispatch_table = DT}) ->
    case proplists:get_val(Host, DT) of
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
    logger:info("Applying routes.", []),
    Dispatch = cowboy_router:compile(DT),
    cowboy:set_env(Listener, dispatch, Dispatch),
    {noreply, State};

handle_cast({add_route, Module, Func, Host, Route}, State = #state{dispatch_table = DT}) ->
    InitialState = #{mod => Module, func => Func},
    RouteInfo = {Route, burbweb_controller, InitialState},

    NewDT =
        case proplists:get_val(Host, DT) of
            undefined ->
                [{Host, [RouteInfo]}|DT];
            {Host, Routes} ->
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
