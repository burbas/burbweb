%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2018 by Niclas Axelsson <niclas@burbas.se>

-module(burbweb_controller_rest).

-export([
         handle/4
        ]).

handle(Mod, Fun, Req = #{method := Method}, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    case Mod:Fun(QsVals, Req) of
	{json, JSON} ->
            EncodedJSON = jsone:encode(JSON, [undefined_as_null]),
            Req1 = cowboy_req:reply(200, #{
                                      <<"content-type">> => <<"application/json">>
                                     }, EncodedJSON, Req),
            {cowboy_rest, Req1, State};
        {json, StatusCode, Headers, JSON} ->
            EncodedJSON = jsone:encode(JSON, [undefined_as_null]),
            Req1 = cowboy_req:reply(StatusCode,
				    maps:join(#{<<"content-type">> =>
						    <<"application/json">>},
					     Headers),
				    EncodedJSON,
				    Req),
            {cowboy_rest, Req1, State};
        {status, StatusCode} when is_integer(StatusCode) ->
            Req1 = cowboy_req:reply(StatusCode, #{}, Req),
            {cowboy_rest, Req1, State}
    end.
