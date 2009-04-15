-module(lua_test).

-include_lib("eunit/include/eunit.hrl").

small_integer_test() ->
    push_to_helper(1, pushinteger, tointeger).
    
zero_integer_test() ->
    push_to_helper(0, pushinteger, tointeger).
    
small_negative_integer_test() ->
    push_to_helper(-2, pushinteger, tointeger).
    

small_number_test() ->
    push_to_helper(2, pushnumber, tonumber).
    
small_negative_number_test() ->
    push_to_helper(-2, pushnumber, tonumber).
    
zero_number_test() ->
    push_to_helper(0, pushnumber, tonumber).

big_number_test() ->
    push_to_helper(5000000000, pushnumber, tonumber).
    
big_floating_number_test() ->
    push_to_helper(5000000000.234, pushnumber, tonumber).
    
big_negative_number_test() ->
    push_to_helper(-5000000000, pushnumber, tonumber).
    
big_negative_float_number_test() ->
    push_to_helper(-5000000000.234, pushnumber, tonumber).


string_test() ->
    push_to_helper("testing", pushstring, tolstring).


call_test() ->
    {ok, L} = lua:new_state(),
    ?assertMatch(ok, lua:getfield(L, global, "type")),
    ?assertMatch(ok, lua:pushnumber(L, 1)),
    ?assertMatch(ok, lua:call(L, 1, 1)),
    ?assertMatch({ok, "number"}, lua:tolstring(L, 1)).
    
set_get_global_test() ->
    {ok, L} = lua:new_state(),
    ?assertMatch(ok, lua:pushnumber(L, 23)),
    ?assertMatch(ok, lua:setfield(L, global, "foo")),
    ?assertMatch(ok, lua:getfield(L, global, "foo")),
    ?assertMatch({ok, 23}, lua:tonumber(L, 1)).


push_to_helper(Val, Push, To) ->
    {ok, L} = lua:new_state(),
    ?assertMatch(ok, lua:Push(L, Val)),
    ?assertMatch({ok, Val}, lua:To(L, 1)).