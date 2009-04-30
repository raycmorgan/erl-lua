-module(lua).

-export([new_state/0,
         close/1,
         call/3,
         concat/2,
         getfield/3,
         getglobal/2,
         gettop/1,
         pushboolean/2,
         pushinteger/2,
         pushstring/2,
         pushnil/1,
         pushnumber/2,
         remove/2,
         setfield/3,
         setglobal/2,
         toboolean/2,
         tointeger/2,
         tolstring/2,
         tonumber/2,
         type/2]).

-include("lua.hrl").
-include("lua_api.hrl").

new_state() ->
    {ok, lua_driver:open()}.
    
close(L) ->
    lua_driver:close(L).

call(L, Args, Results) ->
    command(L, {?ERL_LUA_CALL, Args, Results}),
    receive_simple_response().
    
concat(L, N) ->
    command(L, {?ERL_LUA_CONCAT, N}).

getfield(L, global, Name) ->
    getglobal(L, Name);
getfield(L, Index, Name) ->
    command(L, {?ERL_LUA_GETFIELD, Index, Name}),
    receive_simple_response().
    
getglobal(L, Name) ->
    command(L, {?ERL_LUA_GETGLOBAL, Name}),
    receive_simple_response().

gettop(L) ->
    command(L, {?ERL_LUA_GETTOP}),
    receive_valued_response().
    

pushboolean(L, Bool) ->
    command(L, {?ERL_LUA_PUSHBOOLEAN, Bool}),
    receive_simple_response().
    
pushinteger(L, Int) when is_integer(Int) ->
    command(L, {?ERL_LUA_PUSHINTEGER, Int}),
    receive_simple_response().

pushstring(L, String) when is_list(String) ->
    command(L, {?ERL_LUA_PUSHSTRING, String}),
    receive_simple_response().

pushnil(L) ->
    command(L, {?ERL_LUA_PUSHNIL}),
    receive_simple_response().
    
pushnumber(L, Num) when is_number(Num) ->
    command(L, {?ERL_LUA_PUSHNUMBER, Num}),
    receive_simple_response().

remove(L, Index) ->
    command(L, {?ERL_LUA_REMOVE, Index}),
    receive_simple_response().
    
setfield(L, global, Name) ->
    setglobal(L, Name);
setfield(L, Index, Name) ->
    command(L, {?ERL_LUA_SETFIELD, Index, Name}),
    receive_simple_response().

setglobal(L, Name) ->
    command(L, {?ERL_LUA_SETGLOBAL, Name}),
    receive_simple_response().

toboolean(L, Index) ->
    command(L, {?ERL_LUA_TOBOOLEAN, Index}),
    receive_valued_response().

tointeger(L, Index) ->
    command(L, {?ERL_LUA_TOINTEGER, Index}),
    receive_valued_response().

tolstring(L, Index) ->
    command(L, {?ERL_LUA_TOLSTRING, Index}),
    receive_valued_response().

tonumber(L, Index) ->
    command(L, {?ERL_LUA_TONUMBER, Index}),
    {ok, Value} = receive_valued_response(),
    Value2 = list_to_binary(Value),
    {ok, binary_to_term(Value2)}.

type(L, Index) ->
    command(L, {?ERL_LUA_TYPE, Index}),
    receive_valued_response().


command(#lua{port=Port}, Data) ->
    port_command(Port, term_to_binary(Data)).

receive_simple_response() ->
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
    
receive_valued_response() ->
    receive
        {ok, Str} ->
            {ok, Str};
        error ->
            {error, lua_error};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.