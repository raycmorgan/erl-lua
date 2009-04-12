-module(lual).

-export([dostring/2]).

-include("lua.hrl").
-include("lua_api.hrl").

dostring(#lua{port=Port}, Code) ->
    port_command(Port, term_to_binary({?ERL_LUAL_DOSTRING, Code})),
    recieve_simple_response().

recieve_simple_response() ->
    receive
        ok ->
            ok;
        error ->
            {error, lua_error};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.