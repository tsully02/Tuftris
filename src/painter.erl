%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% painter.erl
%%% 
%%% Painter Module: runs a painter, which receives messages from both the game 
%%%                 room (Game Module) and the client (Tetris Module), and 
%%%                 updates the screen accordingly
%%% 
%%% Painter keeps track of the current state of the board(s), and also draws a
%%% ghost piece (the colorless brackets that represent where your piece will 
%%% fall) for the current player.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(painter).
-include_lib("tetris.hrl").
-export([start/1]).



%%% start/1 waits to receive list of players from the game room and then begins
%%%         the painting loop
-spec start(string()) -> ok.

start(_UserName) ->
    
    receive
        {GameRoom, players, Players} -> 
            GameRoom ! {listener, received_init},
            {Win, Board, Preview} = receive
                {_Pid, winboardprev, W, B, P} -> {W, B, P}
            end,
            % each player now has a tag to indicate if they are in the game
            NewPlayers = new_players(lists:keydelete(self(), 3, Players)),
            Self = {lists:keyfind(self(), 3, Players), Board},
            PlayersList = lists:map(fun (Player) ->
                                        {Player,
                                         board:create(?BOARD_WIDTH,
                                                      ?BOARD_HEIGHT, bg)}
                                    end, NewPlayers),
            paint_players(Self, Preview, PlayersList, Win, GameRoom, 1000, 0)
    end.


%%% paint_players/7 is a listener loop that waits for messages from the game
%%%                 room or the player and updates the physical boards
%%%                 accordingly.
-spec paint_players(tuple(), list(), list(), tuple(), pid(), integer(),
                    integer()) -> ok.

paint_players(Self={{Name, Pid, Listener}, Board}, Preview, Players, Win,
              GameRoom, Speed, NumCleared) ->
    Me = {Name, Pid, Listener},
    receive
        {lebron, jamie} -> ok; % Goated with house sauce, kid perhaps

        % draw the entire screen
        {Pid, draw} ->
            tetris_io:draw_board(Board, Win),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, Win, ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);

        % updates the board for the current player (when a piece gets placed)
        % and redraws it
        {Pid, new_board, NewBoard} -> 
            NewSelf = {Me, NewBoard},
            tetris_io:draw_board(NewBoard, Win),
            cecho:refresh(),
            paint_players(NewSelf, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);
        {GameRoom, clearplayer, PlayerPid} -> 
            RandBoard = board:create_random(?BOARD_WIDTH, ?BOARD_HEIGHT),
            {Idx, _} = get_board(Players, PlayerPid),
            % NewBoard = board:place_piece(PlayerBoard, T),
            BoardWin = get_player_win(Idx, Win),
            tetris_io:draw_board(RandBoard, BoardWin),
            NewPlayers = update_board(Players, PlayerPid, RandBoard),
            paint_players(Self, Preview, player_lost(NewPlayers, PlayerPid),
                          Win, GameRoom, Speed, NumCleared);
        {Pid, clear_row, NewBoard, Rows, Tetromino} ->
            NewSelf = {Me, NewBoard},
            NewPlayers = clear_all_boards(Players, Rows),
            FirstWin = get_player_win(0, Win),
            tetris_io:animate_clear_row(Rows, ?CLEAR_ROW_SLEEP, Win,
                                        ?BOARD_WIDTH),
            tetris_io:draw_board(NewBoard, Win),
            Ghost = tetromino:get_ghost(Tetromino, NewBoard),
            tetris_io:draw_ghost(Ghost, Win, NewBoard),
            tetris_io:draw_tetromino(Tetromino, Win),
            cecho:refresh(),
            % GameRoom ! length(Players),
            Sleep = ?CLEAR_ROW_SLEEP div (length(Players) + 1),
            tetris_io:animate_clear_row(Rows, Sleep, FirstWin,
                                        ?BOARD_WIDTH * length(Players)),
            draw_boards(NewPlayers, Win, GameRoom),
            % tetris_io:draw_board(NewBoard, Win),
            cecho:refresh(),
            Pid ! {self(), continue},
            GameRoom ! {length, length(Rows)},
            {NewSpeed, NewNumCleared} = calc_speed(Speed, NumCleared,
                                                   length(Rows)),
            paint_players(NewSelf, Preview, NewPlayers, Win, GameRoom,
                          NewSpeed, NewNumCleared);

        {Pid, clear_other_rows, Rows} ->
            NewPlayers = clear_all_boards(Players, Rows),
            FirstWin = get_player_win(0, Win),
            Sleep = ?CLEAR_ROW_SLEEP div (length(Players) + 1),
            tetris_io:animate_clear_row(Rows, Sleep, FirstWin,
                                        ?BOARD_WIDTH * length(Players)),
            draw_boards(NewPlayers, Win, GameRoom),
            cecho:refresh(),
            paint_players(Self, Preview, NewPlayers, Win, GameRoom, Speed,
                          NumCleared);

        {Pid, draw_tetromino, OldT, NewT} ->
            OldG = tetromino:get_ghost(OldT, Board),
            tetris_io:delete_tetromino(OldG, Win, Board),
            tetris_io:delete_tetromino(OldT, Win, Board),
            NewG = tetromino:get_ghost(NewT, Board),
            tetris_io:draw_ghost(NewG, Win, Board),
            tetris_io:draw_tetromino(NewT, Win),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);

        {Pid, draw_preview, NewPreview} ->
            tetris_io:draw_preview(NewPreview, Win, bg),
            paint_players(Self, NewPreview, Players, Win, GameRoom, Speed,
                          NumCleared);
        draw -> 
            tetris_io:draw_board(Board, Win),
            draw_boards(Players, Win, GameRoom),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, Win, ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);
        {GameRoom, placepiece, PlayerPid, T} -> 
            {Idx, PlayerBoard} = get_board(Players, PlayerPid),
            NewBoard = board:place_piece(PlayerBoard, T),
            BoardWin = get_player_win(Idx, Win),
            tetris_io:draw_board(NewBoard, BoardWin),
            cecho:refresh(),
            NewPlayers = update_board(Players, PlayerPid, NewBoard),
            paint_players(Self, Preview, NewPlayers, Win, GameRoom, Speed,
                          NumCleared);
        {_RefreshPid, resize} ->
            GameWidth = tetris:get_game_width(length(Players) + 1),
            NewWin = tetris_io:calc_win_coords(GameWidth, ?BOARD_HEIGHT),
            tetris_io:paint_screen(border),
            tetris_io:draw_preview(Preview, NewWin, bg),
            tetris_io:draw_board(Board, NewWin),
            draw_boards(Players, NewWin, GameRoom),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, NewWin,
                ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, NewWin, GameRoom, Speed,
                          NumCleared);
        {Pid, username} ->
            Pid ! {self(), user, Name},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);
        {Pid, end_game, Rankings} ->
            print_end_game_screen(Rankings, Pid);
        {Pid, speed} ->
            Pid ! {self(), speed, Speed},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);
        _Msg ->
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared)
    end.


%%% new_players/1 updates the list of players to include a boolean in the tuple
%%%               to represent if they are still in the game.
-spec new_players(list()) -> list().

new_players([]) -> [];
new_players([{Name, Pid, List} | T]) -> 
    [{Name, Pid, List, true} | new_players(T)].


%%% get_player_win/2 returns the coordinates of the player's board based on
%%%                  their location and which player they are. This does not
%%%                  include self.
-spec get_player_win(integer(), tuple()) -> tuple().

get_player_win(PlayerNum, {Y, X, _, H}) ->
    W = ?BOARD_WIDTH * 2,
    {Y, X + W + 2 + (PlayerNum * W), ?BOARD_WIDTH, H}.


%%% draw_boards/3 takes in the list of players and boards and draws each one,
%%%               updating the player's screen with the new pieces from the
%%%               other players.
-spec draw_boards(list(), tuple(), pid()) -> integer().

draw_boards(PlayersList, {Y, X, Width, Height}, GameRoom) ->
    lists:foldl(fun ({Player, Board}, Col) ->
                    tetris_io:draw_board(Board, {Y, Col, Width, Height}),
                    GameRoom ! {player, Player, col, Col},
                    Col + (?BOARD_WIDTH * 2)
                end, X + (?BOARD_WIDTH * 2) + 2, PlayersList).


%%% get_board/2 retrieves the board and index of the specified player from the
%%%             given list.
-spec get_board(list(), pid()) -> tuple().

get_board(Players, PlayerInfo) -> get_board_r(Players, PlayerInfo, 0).


%%% get_board_r/3 recursively searches the list of players and retrieves the
%%%               board and the index of the player, if they exist.
-spec get_board_r(list(), pid(), integer()) -> tuple().

get_board_r([], _PlayerInfo, _Idx) -> {error, doesnotexist};
get_board_r([{{_, PlayerPid, _, _}, Board} | _T], PlayerPid, Idx) -> 
    {Idx, Board};
get_board_r([_ | T], PlayerPid, Idx) ->
    get_board_r(T, PlayerPid, Idx + 1).


%%% update_board/3 finds the given player and updates their board, returning
%%%                the full updated list of players.
-spec update_board(list(), pid(), list()) -> list().

update_board([], _PlayerPid, _NewBoard) -> [];
update_board([{PlayerInfo={_, PlayerPid, _, _}, _Board} | T],
             PlayerPid, NewBoard) -> 
    [{PlayerInfo, NewBoard} | T];
update_board([H | T], PlayerInfo, NewBoard) ->
    [H | update_board(T, PlayerInfo, NewBoard)].


%%% clear_all_boards/2 clears filled rows across all the boards.
-spec clear_all_boards(list(), list()) -> list().

clear_all_boards([], _Rows) -> [];
clear_all_boards([{Player={_, _, _, true}, Board} | T], Rows) ->
    [{Player, tetris:clear_board_rows(Rows, Board)} |
     clear_all_boards(T, Rows)];
clear_all_boards([{Player={_, _, _, false}, Board} | T], Rows) ->
    [{Player, clear_rand_board_rows(Rows, Board)} | clear_all_boards(T, Rows)].


%%% draw_player_names/3 draws the names of the players underneath their boards.
-spec draw_player_names(list(), tuple(), integer()) -> ok.

draw_player_names([], _, _) -> ok;
draw_player_names([{{Name, _, _, _}, _Board} | PlayerTail], Win, Idx) ->
    PlayerWin = get_player_win(Idx, Win),
    tetris_io:draw_centered_message(?BOARD_HEIGHT, PlayerWin, [Name]),
    draw_player_names(PlayerTail, Win, Idx + 1).


%%% player_lost/2 changes the tuple values to show that they have lost the game.
-spec player_lost(list(), pid()) -> list().

player_lost([], _) -> [];
player_lost([{{Name, Pid, List, _}, Board} | T], Pid) ->
    [{{Name, Pid, List, false}, Board} | player_lost(T, Pid)];
player_lost([H | T], Pid) ->
    [H | player_lost(T, Pid)].


%%% print_end_game_screen/2 prints the final rankings of the game after all
%%%                         players have lost or left the room.
-spec print_end_game_screen(list(), pid()) -> ok.

print_end_game_screen(Rankings, Pid) -> 
    tetris_io:draw_end_game_screen(Rankings),
    receive
        {lebron, jamie} -> ok;
        {_RefreshPid, resize} -> print_end_game_screen(Rankings, Pid)
    end.


%%% clear_rand_board_rows/2 clears filled rows across all the boards.
-spec clear_rand_board_rows(list(), list()) -> list().

clear_rand_board_rows(Rows, Board) ->
    lists:foldl(fun (R, CurrBoard) -> board:remove_rand_row(CurrBoard, R) end,
                Board, Rows).


%%% calc_speed/3 calculates the speed of the falling block based on the overall
%%%              number of rows cleared across the game.
-spec calc_speed(integer(), integer(), integer()) -> tuple().

calc_speed(Speed, NumCleared, RowsCleared) when Speed =< 100 ->
    {Speed, NumCleared + RowsCleared};
calc_speed(Speed, NumCleared, RowsCleared)
        when (NumCleared + RowsCleared) >= 7 ->
    {Speed - 100, (NumCleared + RowsCleared) rem 7};
calc_speed(Speed, NumCleared, RowsCleared) ->
    {Speed, NumCleared + RowsCleared}.
