

-module(server).
-behaviour(gen_server).

-import(string,[equal/2]).

%% Server administration
-export([start_link/0, stop/0]).

% server functions
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% client functions
-export([join_room/4, create_room/5, game_over/2]).

%%%===================================================================
%%% Server Administration
%%%===================================================================

% begins the server so clients and users can create rooms and add chat!
% -spec start_link() -> pid().
start_link() -> gen_server:start_link({local, tetris}, ?MODULE, [], []).

% stops the server, terminating all client processes
% -spec stop() -> ok.
stop() -> gen_server:stop(tetris).

%%%===================================================================
%%% Server Functions
%%%===================================================================

% initializes the state of the server
% -spec init(list()) -> tuple().
init(_Args) -> {ok, []}.

%%%
%%% State:
%%% 
%%% [{RoomName, {Pid, Node}, MaxPlayers, [{PlayerName, {Pid, Node}}]}]


% manages the calls sent to the gen_server, generating replies
% -spec handle_call(term(), pid(), list()) -> ok.

%%% 
%%% Player = {Name, {Pid, Node}}
%%% 
handle_call({newroom, RoomName, NumPlayers, Player}, _From, State) -> 
    IsRoom = is_room(RoomName, State),
    case IsRoom of 
        true -> {reply, already_exists, State};
        _ -> 
            RoomPid = spawn(game, init, [node(), RoomName, NumPlayers, Player]),
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
                    {reply, {RoomInfo, NumPlayers}, NewState}
            end
    end;
handle_call({delete, RoomName}, _From, State) ->
    NewState = delete_room(RoomName, State),
    io:format("Deleting ~p: ~p~n", [RoomName, NewState]),
    {reply, ok, NewState}.


delete_room(_, []) -> [];
delete_room(RoomName, [{RoomName, _, _, _} | T]) ->
    delete_room(RoomName, T);
delete_room(RoomName, [H | T]) ->
    [H | delete_room(RoomName, T)].

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


% terminates all the processes of the server
-spec terminate(tuple(), list()) -> ok.
terminate(_Reason, State) ->
    lists:foreach(fun ({_, RoomPid, _, Players}) ->
                    RoomPid ! stop,
                    lists:foreach(fun ({_, Pid, _}) ->
                                    Pid ! serverdead
                                  end, Players)
                  end, State),
    ok.

%%%===================================================================
%%% Client Functions
%%%===================================================================

% adds the user to the given room running on the given server
join_room(Node, RoomName, Name, ListenerPid) ->
    Server = {tetris, Node},
    Player = {Name, self(), ListenerPid},
    Reply = gen_server:call(Server, {joinroom, RoomName,
                                     {Name, self(), ListenerPid}}),
    case Reply of
        Err when Err == room_full; Err == no_such_room -> Err;
        {Pid, NumPlayers} -> Pid ! {join_room, Player},
             {Pid, NumPlayers}
    end.

create_room(Node, RoomName, Name, NumPlayers, ListenerPid) -> 
    Server = {tetris, Node},
    Pid = gen_server:call(Server, {newroom, RoomName, NumPlayers,
                                   {Name, self(), ListenerPid}}),
    Pid.

game_over(Node, RoomName) ->
    Server = {tetris, Node},
    gen_server:call(Server, {delete, RoomName}).