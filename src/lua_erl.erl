-module(lua_erl).

-export([call/3, call/4]).

-include("lua.hrl").
-include("lua_api.hrl").

call(L, Func, Args) ->
    call(L, Func, Args, 1).
    
call(L, Func, Args, Returns) when is_atom(Func) ->
    call(L, atom_to_list(Func), Args, Returns);
call(L, Func, Args, Returns) when is_list(Args) ->
    [FirstFunc | FuncList] = string:tokens(Func, "."),
    lua:getfield(L, global, FirstFunc),
    lists:foreach(fun(F) ->
            lua:getfield(L, -1, F),
            lua:remove(L, -2)
        end, FuncList),
    lists:foreach(fun(Arg) ->
            push(L, Arg)
        end, Args),
    lua:call(L, length(Args), Returns),
    list_to_tuple(lists:map(fun(I) ->
            R = case lua:type(L, -I) of
                {ok, ?LUA_TNIL} ->
                    nil;
                {ok, ?LUA_TNUMBER} ->
                    {ok, N} = lua:tonumber(L, -I),
                    N;
                {ok, ?LUA_TSTRING} ->
                    {ok, S} = lua:tolstring(L, -I),
                    S
            end,
            lua:remove(L, -I),
            R
        end, lists:seq(Returns, 1, -1))).
    
push(L, Value) when is_number(Value) ->
    lua:pushnumber(L, Value);
push(L, Value) when is_list(Value) ->
    lua:pushstring(L, Value).