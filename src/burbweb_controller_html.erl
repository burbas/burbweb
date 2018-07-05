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
        {ok, Variables} ->
            %% Derive the view from module
            ViewName = atom_to_list(Mod) ++ "_dtl",
            ViewNameAtom = list_to_atom(ViewName),
            handle_view(ViewNameAtom, Variables, #{}, Req, State);
        {ok, Variables, Options} ->
            View =
                case maps:get(view, Options, undefined) of
                    undefined ->
                        ViewName = atom_to_list(Mod) ++ "_dtl",
                        list_to_atom(ViewName);
                    CustomView ->
                        CustomView
                end,
            handle_view(View, Variables, Options, Req, State);
        {status, Status} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, #{}, Req),
            {ok, Req1, State}
    end.


%%%%%%%%%%%%%%%%%%%%%
% Private functions
%%%%%%%%%%%%%%%%%%%%%

handle_view(View, Variables, Options, Req, State) ->
    {ok, HTML} = render_dtl(View, Variables, []),
    Headers =
        case maps:get(headers, Options, undefined) of
            undefined ->
                #{<<"content-type">> => <<"text/html">>};
            UserHeaders ->
                UserHeaders
        end,
    StatusCode = maps:get(status_code, Options, 200),
    Req1 = cowboy_req:reply(StatusCode, Headers, HTML, Req),
    {ok, Req1, State}.


render_dtl(View, Variables, Options) ->
    case code:is_loaded(View) of
        false ->
            %% Cast a warning since the module could not be found
            logger:warning("Could not render ~p cause it's not loaded.", [View]);
        _ ->
            View:render(Variables, Options)
    end.
