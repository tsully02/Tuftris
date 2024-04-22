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

init(ServerInfo, RoomName, NumPlayers, {PlayerName, PlayerPid, PlayerListener}) -> 
    io:format("~p ~p: ~p joined the game!~n", [RoomName, self(), PlayerName]),
    Players = receive_players([{PlayerName, PlayerPid, PlayerListener}], NumPlayers, 1),
    send_message_to_all_listeners({self(), players, Players}, Players),
    send_message_to_all({self(), start}, Players),
    io:format("Num players: ~p~n", [length(Players)]),
    receive_messages(Players, lists:duplicate(?BOARD_HEIGHT, 0), length(Players)),
    send_message_to_all({self(), game_over}, Players),
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
        {join_room, {PlayerName, PlayerPid, Listener}} -> 
            io:format("~p: ~p joined the game!~n", [self(), PlayerName]),
            % Player = {PlayerName, PlayerPid},
            Player = {PlayerName, PlayerPid, Listener},
            NewPlayers = [Player | Players],
            receive_players(NewPlayers, MaxPlayers, NumPlayers + 1);
        M ->
            io:format("Bad message: ~p~n", [M]),
            receive_players(Players, MaxPlayers, NumPlayers)
    end.


delete_num([], _Num) -> [];
delete_num([Num | T], Num) -> 
    delete_num(T, Num);
delete_num([H | T], Num) ->
    [H | delete_num(T, Num)].

check_rows(Players, ClearedRows, Rows, NumCurrPlayers) ->
    UpdatedRows = lists:map(fun ({Idx, Cnt}) ->
                                case lists:member(Idx - 1, ClearedRows) of
                                    true -> Cnt + 1;
                                    false -> Cnt
                                end
                            end,
                            lists:enumerate(Rows)),
    io:format("updaated counts: ~p~n", [UpdatedRows]),
    RowsToSend = lists:filtermap(fun ({Idx, Cnt}) ->
                                    case Cnt of
                                        NumCurrPlayers -> {true, Idx - 1};
                                        _ -> false
                                    end
                                end, lists:enumerate(UpdatedRows)),
    send_message_to_all({clearrow, RowsToSend}, Players),
    io:format("rows cleared: ~p~n", [RowsToSend]),
    NewRows = delete_num(UpdatedRows, NumCurrPlayers),
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
            io:format("Piece placed!~n"),
            send_listener({self(), placepiece, PInfo, T}, Players, PInfo),
            receive_messages(Players, Rows, NumCurrPlayers);
        {playerlost, _PInfo} ->
            NewNum = NumCurrPlayers - 1,
            case NewNum of 
                0 -> ok;
                _ -> NewRows = check_rows(Players, [], Rows, NewNum),
                receive_messages(Players, NewRows, NewNum)
            end;
        {playerquit, PInfo} ->
            NewPlayers = lists:keydelete(PInfo, 2, Players),
            NewNum = NumCurrPlayers - 1,
            case NewNum of 
                0 -> ok;
                _ -> NewRows = check_rows(NewPlayers, [], Rows, NewNum),
                receive_messages(NewPlayers, NewRows, NewNum)
            end;
        stop -> ok;
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            receive_messages(Players, Rows, NumCurrPlayers)
    end.
