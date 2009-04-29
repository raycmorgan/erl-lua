Erl-Lua is a library for embedding Lua into Erlang. It provides a simple interface that is very similar to the Lua C API. In the future it will also include a higher level API to simplify things further.

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