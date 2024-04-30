%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% game.erl
%%% 
%%% Game Module: contains implementation of a game room that connects all 
%%%              players in a game
%%% 
%%% Important Data Structures
%%%     - Player: {Name, ClientPid, PainterPid}
%%%     - Players: list containing the above struct
%%%     - RowsCleared: list of list of pids
%%%         The elements of the outer list represents a row in the board, and 
%%%         that element holds all of the pids that have cleared that row
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(game).

-include_lib("tetris.hrl").

% Game room code

-export([init/4]).

-type player() :: {list(integer()), pid(), pid()}.
-type row_clear_list() :: [[pid()],...].
-export_type([player/0]).


%%% init called by server
%%%     starts game room
%%%     adds first player
%%%     calls wait_for_game type function
%%%     calls start_game type function
%%% 
%%% wait_for_game
%%%     listens for messages from server, waiting for all players to enter
%%%     the game room
%%% 
%%% start_game
%%%     enters game loop, listening for messages from players
%%%
%%% check_rows


%%% init/4
%%% 
%%% starts up the game room--waits for more players to join, and starts the 
%%% game, and returns when the game is over
-spec init(atom(), list(integer()), integer(), player()) -> ok.

init(ServerInfo, RoomName, NumPlayers,
     First={_PlayerName, _PlayerPid, _PlayerPainter}) -> 
    io:format("~p ~p: ~p joined the game!~n", [RoomName, self(), First]),
    Players = receive_players([First], NumPlayers, 1),
    send_message_to_all_painters({self(), players, Players}, Players),
    send_message_to_all({self(), start}, Players),
    io:format("Num players: ~p~n", [length(Players)]),
    io:format("Players: ~p~n", [Players]),
    {NewPlayers, Rankings} =
        receive_messages(Players, lists:duplicate(?BOARD_HEIGHT, []),
                         length(Players), []),
    io:format("Game over! ~p~n", [NewPlayers]),
    send_message_to_all({self(), game_over, Rankings}, NewPlayers),
    server:game_over(ServerInfo, RoomName),
    ok.


%%% send_message_to_all_painters/2 sends a message to every painter
-spec send_message_to_all_painters(any(), [player()]) -> ok.

send_message_to_all_painters(Message, Players) ->
    lists:foreach(fun ({_, _, Pid}) -> Pid ! Message end, Players).


%%% send_message_to_all/2 sends a message to every client
-spec send_message_to_all(any(), [player()]) -> ok.

send_message_to_all(Message, Players) ->
    lists:foreach(fun ({_, Pid, _}) -> Pid ! Message end, Players).


%%% send_painter/3 sends a message to every painter except one (generally the
%%%                client the message came from). The from argument should be
%%%                the client PID to exclude, NOT the painter PID
-spec send_painter(any(), [player(),...], pid()) -> ok.

send_painter(Message, [{_, From, _} | Tail], From) ->
    lists:foreach(fun ({_, _, Pid}) -> Pid ! Message end, Tail);
send_painter(Message, [{_, _Head, Painter} | Tail], From) ->
    Painter ! Message,
    send_painter(Message, Tail, From).


%%% receive_players/3 waits until it has received info for MaxPlayers players,
%%%                   then returns the list of players received
-spec receive_players([player()], pos_integer(), pos_integer()) -> [player()].

receive_players(Players, MaxPlayers, MaxPlayers) ->
    Players;
receive_players(Players, MaxPlayers, NumPlayers) ->
    receive 
        {join_room, Player={_PlayerName, _PlayerPid, _Painter}} -> 
            io:format("~p: ~p joined the game!~n", [self(), Player]),
            NewPlayers = [Player | Players],
            receive_players(NewPlayers, MaxPlayers, NumPlayers + 1);
        M ->
            io:format("Bad message: ~p~n", [M]),
            receive_players(Players, MaxPlayers, NumPlayers)
    end.


%%% check_rows/5 checks for cleared rows and messages clients about them
%%% Arguments:
%%%     Players: List of players
%%%     ClearedRows: List of row numbers that a player just cleared
%%%     ClearPlayer: The player who cleared the rows in ClearedRows
%%%     Rows: The existing row clear state
%%%     NumCurrPlayers: The number of players currently in the game
-spec check_rows([player()], [integer()], pid(), row_clear_list(),
                 integer()) -> row_clear_list().

check_rows(Players, ClearedRows, ClearPlayer, Rows, NumCurrPlayers) ->
    % Add ClearPlayer to lists whose indices are specified in ClearedRows
    % Boards are 0-indexed but enumerate is 1-indexed
    UpdatedRows = lists:map(fun ({Idx, ClearedPlayers}) ->
                                case lists:member(Idx - 1, ClearedRows) of
                                    true -> [ClearPlayer | ClearedPlayers];
                                    false -> ClearedPlayers
                                end
                            end,
                            lists:enumerate(Rows)),
    io:format("updated row clears: ~p~n", [UpdatedRows]),
    RowsToSend = lists:filtermap(fun ({Idx, ClearedPlayers}) ->
                                    case length(ClearedPlayers) of
                                        NumCurrPlayers -> {true, Idx - 1};
                                        _ -> false
                                    end
                                end, lists:enumerate(UpdatedRows)),
    send_message_to_all({clearrow, RowsToSend}, Players),
    io:format("rows cleared: ~p~n", [RowsToSend]),
    NewRows = lists:filter(fun (RowPlayers) ->
                                length(RowPlayers) /= NumCurrPlayers
                           end, UpdatedRows),
    io:format("new rows: ~p~n", [NewRows]),
    lists:append(lists:duplicate(?BOARD_HEIGHT - length(NewRows), []), NewRows).


%%% delete_player/2 removes a player from all clear row tracking lists
-spec delete_player(pid(), row_clear_list()) -> row_clear_list().

delete_player(Pid, Rows) ->
    lists:map(fun (PlayerPids) -> lists:delete(Pid, PlayerPids) end, Rows).


%%% receive_messages/4 acts as the main loop for the game room process
%%% Arguments:
%%%     Players: List of players
%%%     Rows: Row clear tracking list
%%%     NumCurrPlayers: Current number of active players
%%%     NotPlaying: List of {Name, Pid} pairs for players observing the game
-spec receive_messages([player(),...], row_clear_list(), integer(),
                       [{[integer()], pid()}]) -> {[player()],
                                                   [{[integer()], pid()}]}.

receive_messages(Players, Rows, NumCurrPlayers, NotPlaying) ->
    receive
        {rowcleared, ClearedRows, PInfo} ->
            NewRows = check_rows(Players, ClearedRows, PInfo, Rows,
                                 NumCurrPlayers),
            receive_messages(Players, NewRows, NumCurrPlayers, NotPlaying);
        {placepiece, T, PInfo} ->
            io:format("Piece placed!~n"),
            send_painter({self(), placepiece, PInfo, T}, Players, PInfo),
            receive_messages(Players, Rows, NumCurrPlayers, NotPlaying);
        {playerlost, PInfo} ->
            NewNum = NumCurrPlayers - 1,
            case NewNum of 
                0 -> {Players, [get_name_pid(Players, PInfo) | NotPlaying]};
                1 -> {Players, add_winner(Players,
                                          [get_name_pid(Players, PInfo) |
                                           NotPlaying])};
                _ -> send_painter({self(), clearplayer, PInfo}, Players,
                                   PInfo),
                NewRows = check_rows(Players, [], PInfo,
                                     delete_player(PInfo, Rows), NewNum),
                receive_messages(Players, NewRows, NewNum,
                                 [get_name_pid(Players, PInfo) | NotPlaying])
            end;
        {playerquit, PInfo} ->
            NewPlayers = lists:keydelete(PInfo, 2, Players),
            Exists = lists:keyfind(get_name_pid(Players, PInfo), 2,
                                   lists:enumerate(NotPlaying)),
            {NewNum, NewNotPlaying} =
                case Exists of 
                    false -> send_painter({self(), clearplayer, PInfo}, Players,
                                          PInfo),
                             {NumCurrPlayers - 1, [PInfo | NotPlaying]};
                    _ -> io:format("Already lost!~n"),
                         {NumCurrPlayers, NotPlaying}
                end,
            io:format("Num players: ~p~n", [NewNum]),
            case NewNum of 
                0 -> {NewPlayers, NewNotPlaying};
                1 -> {NewPlayers, add_winner(Players, NewNotPlaying)};
                _ -> NewRows = check_rows(NewPlayers, [], PInfo,
                                          delete_player(PInfo, Rows), NewNum),
                               receive_messages(NewPlayers, NewRows, NewNum,
                                                NewNotPlaying)
            end;
        stop -> {Players, NotPlaying};
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            receive_messages(Players, Rows, NumCurrPlayers, NotPlaying)
    end.


%%% add_winner/2 returns the list of losers with the winner at the front
-spec add_winner([player()], [{[integer()], pid()}]) -> [{[integer()], pid()}].

add_winner(Players, Losers) ->
    io:format("Players: ~p~n, Losers: ~p~n", [Players, Losers]),
    [{WName, WPid, _}] = lists:foldl(fun ({_, P}, Acc) ->
                                        lists:keydelete(P, 2, Acc)
                                     end, Players, Losers),
    [{WName, WPid} | Losers].


%%% get_name_pid/2 gets a {name, pid} pair from a pid given the player list
-spec get_name_pid([player()], pid()) -> {[integer()], pid()}.

get_name_pid([], _) -> {error, doesnotexist};
get_name_pid([{Name, Pid, _} | _], Pid) ->
    {Name, Pid};
get_name_pid([_ | T], Pid) ->
    get_name_pid(T, Pid).
