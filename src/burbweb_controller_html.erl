%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2018 by Niclas Axelsson <niclas@burbas.se>

-module(burbweb_controller_html).

-export([
         handle/4
        ]).

handle(Mod, Fun, Req = #{method := Method, path := Path}, State) ->
    case Mod:Fun(Method, Path, Req) of
        {json, JSON} ->
            EncodedJSON = jsone:encode(JSON, [undefined_as_null]),
            Req1 = cowboy_req:reply(200, #{
                                      <<"content-type">> => <<"application/json">>
                                     }, EncodedJSON, Req),
            {ok, Req1, State};
        {ok, View, Variables} ->
            %% Check if the view have been compiled and loaded
            {ok, HTML} = render_dtl(View, Variables, []),
            Req1 = cowboy_req:reply(200, #{
                                      <<"content-type">> => <<"application/html">>
                                     }, HTML, Req),
            {ok, Req1, State};
        {status, Status} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, #{}, Req),
            {ok, Req1, State}
    end.


render_dtl(View, Variables, Options) ->
    case code:is_loaded(View) of
        false ->
            %% See if we can compile it?
            ok;
        _ ->
            View:render(Variables, Options)
    end.
