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
    Players = receive_players([{PlayerName, PlayerPid, board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, ?SCREEN_BGD_COLOR)}], NumPlayers, 1),
    Listener = spawn(fun () -> receive_messages(Players) end),
    send_message_to_all({Listener, start}, Players),
    io:format("~p~n", [Players]),
    ok.

send_message_to_all(Message, Players) ->
    lists:foreach(fun ({_, Pid, _}) -> Pid ! Message end, Players).

send_message(Message, [{_, From, _} | Tail], From) ->
    lists:foreach(fun ({_, Pid, _}) -> Pid ! Message end, Tail);
send_message(Message, [{_, Head, _} | Tail], From) ->
    Head ! Message,
    send_message(Message, Tail, From).

receive_players(Players, MaxPlayers, MaxPlayers) ->
    Players;
receive_players(Players, MaxPlayers, NumPlayers) ->
    receive 
        {join_room, {PlayerName, PlayerPid}} -> 
            io:format("~p: ~p joined the game!~n", [self(), PlayerName]),
            % Player = {PlayerName, PlayerPid},
            Player = {PlayerName, PlayerPid, board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, ?SCREEN_BGD_COLOR)},
            NewPlayers = [Player | Players],
            receive_players(NewPlayers, MaxPlayers, NumPlayers + 1);
        M ->
            io:format("Bad message: ~p~n", [M]),
            receive_players(Players, MaxPlayers, NumPlayers)
    end.

receive_messages(Players, Rows) ->
    receive
        {newpiece, T, PInfo} ->
            io:format("sending piece message~n", []),
            send_message({newpiece, PInfo, T}, Players, PInfo),
            receive_messages(Players);
        % {rowcleared, Row, PInfo} ->
            
        % {placepiece, T, PInfo} ->
        %     send_message({placepiece, PInfo, T}, Players, PInfo)
        stop -> ok;
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            receive_messages(Players)
    end.


% TODO: player enters room but leaves before the game has begun
% TODO: board representation for each player
% TODO: which pid should go in the server state? init or listener??
% TODO: when user quits or leaves, tell game (or loses)
% TODO: when last user quits or leaves, stop game (or loses)
% TODO: fix listener issue? changing order within init funciton to avoid spawning process