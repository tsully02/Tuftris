-module(game).

% -include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([init/3]).


%%% init called by server
%%%     starts game room
%%%     adds first player
%%%     calls wait_for_game type function
%%%     calls start_game type function
%%% 
%%% wait_for_game
%%%     listens for messages from server, waiting for all players to enter the game room
%%% 
%%% start_game
%%%     enters game loop, listening for messages from players
%%%
%%% check_rows
%%% 
%%% 
%%% 
%%%
%%%

init(RoomName, NumPlayers, {PlayerName, PlayerPid}) -> 
    io:format("~p ~p: ~p joined the game!~n", [RoomName, self(), PlayerName]),
    Players = receive_players([{PlayerName, PlayerPid}], NumPlayers, 1),
    % Listener = spawn(fun () -> receive_messages(Players) end),
    send_message_to_all({self(), start}, Players),
    io:format("Num players: ~p~n", [length(Players)]),
    receive_messages(Players, lists:duplicate(?BOARD_HEIGHT, 0), length(Players)),
    ok.

send_message_to_all(Message, Players) ->
    lists:foreach(fun ({_, Pid}) -> Pid ! Message end, Players).

send_message(Message, [{_, From} | Tail], From) ->
    lists:foreach(fun ({_, Pid}) -> Pid ! Message end, Tail);
send_message(Message, [{_, Head} | Tail], From) ->
    Head ! Message,
    send_message(Message, Tail, From).

receive_players(Players, MaxPlayers, MaxPlayers) ->
    Players;
receive_players(Players, MaxPlayers, NumPlayers) ->
    receive 
        {join_room, {PlayerName, PlayerPid}} -> 
            io:format("~p: ~p joined the game!~n", [self(), PlayerName]),
            % Player = {PlayerName, PlayerPid},
            Player = {PlayerName, PlayerPid},
            NewPlayers = [Player | Players],
            receive_players(NewPlayers, MaxPlayers, NumPlayers + 1);
        M ->
            io:format("Bad message: ~p~n", [M]),
            receive_players(Players, MaxPlayers, NumPlayers)
    end.


check_rows(Players, ClearedRows, Rows, NumCurrPlayers) ->
    UpdatedRows = lists:map(fun ({Idx, Cnt}) ->
                                case lists:member(Idx, ClearedRows) of
                                    true -> Cnt + 1;
                                    false -> Cnt
                                end
                            end,
                            lists:enumerate(Rows)),
    io:format("updaated counts: ~p~n", [UpdatedRows]),
    RowsToSend = lists:filtermap(fun ({Idx, Cnt}) ->
                                    case Cnt of
                                        NumCurrPlayers -> {true, Idx};
                                        _ -> false
                                    end
                                end, lists:enumerate(UpdatedRows)),
    send_message_to_all({clearrow, RowsToSend}, Players),
    io:format("rows cleared: ~p~n", [RowsToSend]),
    NewRows = lists:delete(NumCurrPlayers, UpdatedRows),
    io:format("new rows: ~p~n", [NewRows]),
    lists:append(lists:duplicate(?BOARD_HEIGHT - length(NewRows), length(Players) - NumCurrPlayers), NewRows).

receive_messages(Players, Rows, NumCurrPlayers) ->
    receive
        {newpiece, T, PInfo} ->
            % io:format("new piece!~n", []),
            send_message({newpiece, PInfo, T}, Players, PInfo),
            receive_messages(Players, Rows, NumCurrPlayers);
        {rowcleared, ClearedRows, _PInfo} ->
            % io:format("clearing row ~p~n", [ClearedRows]),
            NewRows = check_rows(Players, ClearedRows, Rows, NumCurrPlayers),
            receive_messages(Players, NewRows, NumCurrPlayers);
        {placepiece, T, PInfo} ->
            % io:format("Piece placed!~n"),
            send_message({placepiece, PInfo, T}, Players, PInfo),
            receive_messages(Players, Rows, NumCurrPlayers);
        stop -> ok;
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            receive_messages(Players, Rows, NumCurrPlayers)
    end.


% TODO: player enters room but leaves before the game has begun
% TODO: when user quits or leaves, tell game (or loses)
% TODO: when last user quits or leaves, stop game (or loses)