-module(game).

% -include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([init/4]).


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

init(ServerInfo, RoomName, NumPlayers, First={_PlayerName, _PlayerPid, _PlayerListener}) -> 
    io:format("~p ~p: ~p joined the game!~n", [RoomName, self(), First]),
    Players = receive_players([First], NumPlayers, 1),
    send_message_to_all_listeners({self(), players, Players}, Players),
    send_message_to_all({self(), start}, Players),
    io:format("Num players: ~p~n", [length(Players)]),
    io:format("Players: ~p~n", [Players]),
    NewPlayers = receive_messages(Players, lists:duplicate(?BOARD_HEIGHT, []), length(Players), []),
    io:format("Game over! ~p~n", [NewPlayers]),
    send_message_to_all({self(), game_over}, NewPlayers),
    server:game_over(ServerInfo, RoomName),
    ok.

send_message_to_all_listeners(Message, Players) ->
    lists:foreach(fun ({_, _, Pid}) -> Pid ! Message end, Players).

send_message_to_all(Message, Players) ->
    lists:foreach(fun ({_, Pid, _}) -> Pid ! Message end, Players).

send_message(Message, [{_, From, _} | Tail], From) ->
    lists:foreach(fun ({_, Pid, _}) -> Pid ! Message end, Tail);
send_message(Message, [{_, Head, _} | Tail], From) ->
    Head ! Message,
    send_message(Message, Tail, From).

send_listener(Message, [{_, From, _} | Tail], From) ->
    lists:foreach(fun ({_, _, Pid}) -> Pid ! Message end, Tail);
send_listener(Message, [{_, _Head, Listener} | Tail], From) ->
    Listener ! Message,
    send_listener(Message, Tail, From).

receive_players(Players, MaxPlayers, MaxPlayers) ->
    Players;
receive_players(Players, MaxPlayers, NumPlayers) ->
    receive 
        {join_room, Player={_PlayerName, _PlayerPid, _Listener}} -> 
            io:format("~p: ~p joined the game!~n", [self(), Player]),
            % Player = {PlayerName, PlayerPid},
            % Player = {PlayerName, PlayerPid, Listener},
            NewPlayers = [Player | Players],
            receive_players(NewPlayers, MaxPlayers, NumPlayers + 1);
        M ->
            io:format("Bad message: ~p~n", [M]),
            receive_players(Players, MaxPlayers, NumPlayers)
    end.

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
    NewRows = lists:filter(fun (RowPlayers) -> length(RowPlayers) /= NumCurrPlayers end, UpdatedRows),
    io:format("new rows: ~p~n", [NewRows]),
    lists:append(lists:duplicate(?BOARD_HEIGHT - length(NewRows), []), NewRows).

delete_player(Pid, Rows) ->
    lists:map(fun (PlayerPids) -> lists:delete(Pid, PlayerPids) end, Rows).

receive_messages(Players, Rows, NumCurrPlayers, NotPlaying) ->
    receive
        % {newpiece, T, PInfo} ->
        %     % io:format("new piece!~n"),
        %     send_message({newpiece, PInfo, T}, Players, PInfo),
        %     receive_messages(Players, Rows, NumCurrPlayers, NotPlaying);
        {rowcleared, ClearedRows, PInfo} ->
            % io:format("clearing row ~p~n", [ClearedRows]),
            NewRows = check_rows(Players, ClearedRows, PInfo, Rows, NumCurrPlayers),
            receive_messages(Players, NewRows, NumCurrPlayers, NotPlaying);
        {placepiece, T, PInfo} ->
            io:format("Piece placed!~n"),
            send_listener({self(), placepiece, PInfo, T}, Players, PInfo),
            receive_messages(Players, Rows, NumCurrPlayers, NotPlaying);
        {playerlost, PInfo} ->
            NewNum = NumCurrPlayers - 1,
            case NewNum of 
                0 -> Players;
                _ -> NewRows = check_rows(Players, [], PInfo, delete_player(PInfo, Rows), NewNum),
                receive_messages(Players, NewRows, NewNum, [PInfo | NotPlaying])
            end;
        {playerquit, PInfo} ->
            NewPlayers = lists:keydelete(PInfo, 2, Players),
            Exists = lists:keyfind(PInfo, 2, lists:enumerate(NotPlaying)),
            NewNum = case Exists of 
                false -> NumCurrPlayers - 1;
                _ -> NumCurrPlayers
            end,
            case NewNum of 
                0 -> NewPlayers;
                _ -> NewRows = check_rows(NewPlayers, [], PInfo, delete_player(PInfo, Rows), NewNum),
                receive_messages(NewPlayers, NewRows, NewNum, NotPlaying)
            end;
        stop -> ok;
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            receive_messages(Players, Rows, NumCurrPlayers, NotPlaying)
    end.
