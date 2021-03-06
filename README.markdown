Erl-Lua is a library for embedding Lua into Erlang. It provides a simple interface that is very similar to the Lua C API. In the future it will also include a higher level API to simplify things further.

WARNING: This is definitely not fully tested. Still a bunch of work to be done. If you are careful though, it should be pretty stable (no promises though).

Example:

    {ok, L} = lua:new_state().
    lua:getfield(L, global, "print").
    lua:pushstring(L, "Hello from Lua!").
    lua:call(L, 1, 0).
    % (Lua) => Hello from Lua!

    lua:getfield(L, global, "type").
    lua:pushnumber(L, 23).
    lua:call(L, 1, 1).
    {ok, S} = lua:tolstring(L, 1).
    lua:remove(L, 1). % always rebalance the stack.. it is the right thing to do!
    S. % => "number" 

For more examples, see the tests.

There is also a simple way to run one off simple Lua code snippets:

    (continued from above)
    lual:dostring(L, "print 'Howdy!'").
    % (Lua) => Howdy!
    
**NEW Higher Level API**

*call* (lua\_state L, (atom|string) function\_name, list arguments, [int num\_returned]) - Call a Lua function and return the values.

    1> {ok, L} = lua:new_state().
    2> lua_erl:call(L, type, [23], 1).
    {"number"}
    3> lual:dostring(L, "function add(a, b, c) return a + b + c end").
    4> lua_erl:call(L, add, [2, 3, 4], 1).
    {9}

The strange 4th arg is the number of values the function can return (since in Lua you can return multiple things).
If the number of returned values is 1, the argument can be left off.. therefore both of the above could be rewritten:

    lua_erl:call(L, type, [23]).
    lua_erl:call(L, add, [2, 3, 4]).