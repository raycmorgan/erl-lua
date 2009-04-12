-module(lua_test).

-include_lib("eunit/include/eunit.hrl").

string_test() ->
    {ok, L} = lua:new_state(),
    ?assert(lua:pushstring(L, "testing") == ok),
    ?assert(lua:tolstring(L, 1) == {ok, "testing"}).