-module(game).

% -include_lib("../cecho/include/cecho.hrl").
% -include_lib("tetris.hrl").

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

init(RoomName, NumPlayers, {PlayerName, {_PlayerPid, _Node}}) -> 
    io:format("~p ~p: ~p joined the game!", [RoomName, self(), PlayerName]),
    Players = receive_players([{PlayerName, {_PlayerPid, _Node}}], NumPlayers, 1),
    io:format("~p~n", [Players]),
    ok.

receive_players(Players, MaxPlayers, MaxPlayers) ->
    Players;
receive_players(Players, MaxPlayers, NumPlayers) ->
    receive 
        {join_room, {PlayerName, {PlayerPid, Node}}} -> 
            io:format("~p: ~p joined the game!~n", [self(), PlayerName]),
            Player = {PlayerName, {PlayerPid, Node}},
            NewPlayers = [Player | Players],
            receive_players(NewPlayers, MaxPlayers, NumPlayers + 1);
        M -> io:format("Bad message: ~p~n", [M])
    end.



% TODO: player enters room but leaves before the game has begun