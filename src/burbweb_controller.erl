%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2018 by Niclas Axelsson <niclas@burbas.se>

-module(burbweb_controller).

-export([
         init/2,
         terminate/3
        ]).

init(Req, State = #{secure := false}) -> dispatch(Req, State);
init(Req, State = #{secure := {Mod, Func}}) ->
    case Mod:Func(Req) of
        true ->
            dispatch(Req, State);
        _ ->
            Req1 = cowboy_req:reply(401, Req),
            {ok, Req1, State}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


dispatch(Req = #{method := ReqMethod},
         State = #{mod := Mod, func := Func, method := Method, type := Type}) when Method == '_' orelse
                                                                                   Method == ReqMethod ->
    case Type of
	rest ->
	    %% Initiate REST protocol
	    burbweb_controller_rest:handle(Mod, Func, Req, State);
	html ->
	    %% Initiate the basic protocol
	    burbweb_controller_html:handle(Mod, Func, Req, State);
	websocket ->
	    %% Websocket
	    {cowboy_websocket, Req, State}
    end;
dispatch(Req, State) ->
    Req1 = cowboy_req:reply(404, Req),
    {ok, Req1, State}.



terminate(_Reason, _Req, _State) ->
    ok.
