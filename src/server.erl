

-module(server).
-behaviour(gen_server).

-import(string,[equal/2]).

%% Server administration
-export([start_link/0, stop/0]).

% server functions
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% client functions
-export([join_room/3, create_room/4]).

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

%%%
%%% State:
%%% 
%%% [{RoomName, {Pid, Node}, MaxPlayers, [{PlayerName, {Pid, Node}}]}]


% manages the calls sent to the gen_server, generating replies
-spec handle_call(term(), pid(), list()) -> ok.

%%% 
%%% Player = {Name, {Pid, Node}}
%%% 
handle_call({newroom, RoomName, NumPlayers, Player}, _From, State) -> 
    IsRoom = is_room(RoomName, State),
    case IsRoom of 
        true -> {reply, already_exists, State};
        _ -> 
            RoomPid = spawn(game, init, [RoomName, NumPlayers, Player]),
            io:format("Game Room Pid: ~p~n", [RoomPid]),
            Room = {RoomName, RoomPid, NumPlayers, [Player]},
            {reply, RoomPid, [Room | State]}
    end;
handle_call({joinroom, RoomName, Player}, _From, State) -> 
    Room = get_room(RoomName, State),
    case Room of
        false -> {reply, no_such_room, State};
        _ -> 
            {RoomName, RoomInfo, NumPlayers, PlayerList} = Room,
            case length(PlayerList) of 
                NumPlayers -> {reply, room_full, State};
                _ -> 
                    NewState = add_player(RoomName, Player, State),
                    {reply, RoomInfo, NewState}
            end
    end. 
            

is_room(RoomName, State) ->
    lists:any(fun ({Room, _, _, _}) -> equal(Room, RoomName) end, State).

get_room(RoomName, State) ->
    lists:keyfind(RoomName, 1, State).

add_player(RoomName, Player, [{RoomName, RoomInfo, NumP, PlayerList} | T]) -> 
    [{RoomName, RoomInfo, NumP, [Player | PlayerList]} | T];
add_player(RoomName, Player, [H | T]) -> 
    [H | add_player(RoomName, Player, T)].
    

% manages the casts sent to the gen_server, updating the state as necessary
-spec handle_cast(tuple() | atom(), list()) -> tuple().
handle_cast(stop, State) ->
    {stop, normal, State}.
% handle_cast({subscribe, RoomName, User}, State) -> 
%     io:format("User ~p subscribed!~n", [User]),
%     case lists:keyfind(RoomName, 1, State) of
%         {RoomName, Users} -> NewState = lists:delete({RoomName, Users}, State),
%                              {noreply, [{RoomName, [User | Users]} | NewState]};
%         false             -> {noreply, [{RoomName, [User]} | State]}
%     end;
% handle_cast({unsubscribe, RoomName, User}, State) ->
%     case lists:keyfind(RoomName, 1, State) of
%         {RoomName, Users} -> NewUsers = lists:delete(User, Users),
%                              NewState = lists:delete({RoomName, Users}, State),
%                              io:format("Unsubscribing user ~p~n", [User]),
%                              {noreply, [{RoomName, NewUsers} | NewState]};
%         false             -> io:format("error: user does not exist"),
%                              {noreply, State}
%     end.


% terminates all the processes of the server
-spec terminate(tuple(), list()) -> ok.
terminate(_Reason, _State) ->
    % lists:foreach(fun ({_, Users}) ->
    %     lists:foreach(fun ({_, Pid}) -> Pid ! quitting end, Users) end,
    %     State),
    ok.

%%%===================================================================
%%% Client Functions
%%%===================================================================

% adds the user to the given room running on the given server
-spec join_room(atom(), string(), string()) -> ok.
join_room(Node, RoomName, Name) ->
    Server = {tetris, Node},
    Player = {Name, self()},
    Pid = gen_server:call(Server, {joinroom, RoomName, {Name, self()}}),
    case Pid of
        Err when Err == room_full; Err == no_such_room -> Err;
        _ -> Pid ! {join_room, Player}, Pid
    end.

create_room(Node, RoomName, Name, NumPlayers) -> 
    Server = {tetris, Node},
    Pid = gen_server:call(Server, {newroom, RoomName, NumPlayers, {Name, self()}}),
    Pid.



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
