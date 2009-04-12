-module(lua_server).
-author("RayMorgan").

-behaviour(gen_server).

-export([start/0, call/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server
%
%% @spec start_link() -> {ok,pid()} | ignore | {error,Error::term()}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start(?MODULE, [], []).

call(Pid, Command, Args) ->
    gen_server:call(Pid, {call, Command, Args}).


%%====================================================================
%% gen_server Callback Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Initiates the server
%%
%% @spec init(Args::term()) -> {ok, State} |
%%                         {ok, State, Timeout::integer()} |
%%                         ignore               |
%%                         {stop, Reason::term()}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    L = lua_driver:open(),
    {ok, L}.

%%--------------------------------------------------------------------
%% @doc Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call, Command, Args}, _From, L) ->
    Reply = apply(lua_api, Command, [L | Args]),
    {reply, Reply, L}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------