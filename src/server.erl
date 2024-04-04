

-module(server).
-behaviour(gen_server).

%% Server administration
-export([start_link/0, stop/0]).

% server functions
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% client functions
-export([join_room/3]).

%%%===================================================================
%%% Server Administration
%%%===================================================================

% begins the server so clients and users can create rooms and add chat!
-spec start_link() -> pid().
start_link() -> gen_server:start_link({local, tetris}, ?MODULE, [], []).

% stops the server, terminating all client processes
-spec stop() -> ok.
stop() -> gen_server:stop(tetris).

%%%===================================================================
%%% Server Functions
%%%===================================================================

% initializes the state of the server
-spec init(list()) -> tuple().
init(_Args) -> {ok, []}.


% manages the calls sent to the gen_server, generating replies
-spec handle_call(term(), pid(), list()) -> ok.
handle_call(_Request, _From, _State) -> ok.


% manages the casts sent to the gen_server, updating the state as necessary
-spec handle_cast(tuple() | atom(), list()) -> tuple().
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({subscribe, RoomName, User}, State) -> 
    io:format("User ~p subscribed!~n", [User]),
    case lists:keyfind(RoomName, 1, State) of
        {RoomName, Users} -> NewState = lists:delete({RoomName, Users}, State),
                             {noreply, [{RoomName, [User | Users]} | NewState]};
        false             -> {noreply, [{RoomName, [User]} | State]}
    end;
handle_cast({unsubscribe, RoomName, User}, State) ->
    case lists:keyfind(RoomName, 1, State) of
        {RoomName, Users} -> NewUsers = lists:delete(User, Users),
                             NewState = lists:delete({RoomName, Users}, State),
                             io:format("Unsubscribing user ~p~n", [User]),
                             {noreply, [{RoomName, NewUsers} | NewState]};
        false             -> io:format("error: user does not exist"),
                             {noreply, State}
    end.


% terminates all the processes of the server
-spec terminate(tuple(), list()) -> ok.
terminate(_Reason, State) ->
    lists:foreach(fun ({_, Users}) ->
        lists:foreach(fun ({_, Pid}) -> Pid ! quitting end, Users) end,
        State),
    ok.

%%%===================================================================
%%% Client Functions
%%%===================================================================

% adds the user to the given room running on the given server
-spec join_room(atom(), string(), string()) -> ok.
join_room(Node, RoomName, UserName) ->
    Server = {tetris, Node},
    Listener = spawn(fun () -> receive_messages(Server, RoomName) end),
    gen_server:cast(Server, {subscribe, RoomName, {UserName, Listener}}),
    send_messages(RoomName, Server),
    gen_server:cast(Server, {unsubscribe, RoomName, {UserName, Listener}}),
    Listener ! done,
    ok.


% loop for listener process to receive messages
-spec receive_messages(tuple(), string()) -> ok.
receive_messages(Server, RoomName) ->
    receive
        done ->
            ok;
        quitting ->
            io:format("Server has stopped; type \"--quit\" now to exit chat~n"),
            ok;
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            receive_messages(Server, RoomName)
    end.


% loop for sending messages so user can chat
-spec send_messages(string(), tuple()) -> ok.
send_messages(_RoomName, _Server) -> ok.
