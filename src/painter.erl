-module(painter).
-include_lib("tetris.hrl").
-export([start/1]).



%%% 
%%% Players list:
%%% [{{Name, Pid, Painter}, Board} | Players]
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
            % GameRoom ! {playerlist, PlayersList},
            paint_players(Self, Preview, PlayersList, Win, GameRoom, 1000, 0)
    end.

%%% paint_players/7
paint_players(Self={{Name, Pid, Listener}, Board}, Preview, Players, Win,
              GameRoom, Speed, NumCleared) ->
    Me = {Name, Pid, Listener},
    receive
        {lebron, jamie} -> ok; % Goated with house sauce, kid perhaps
        {Pid, draw} ->
            tetris_io:draw_board(Board, Win),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, Win, ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);
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
            % io:format("HOWDY~n"),
            print_end_game_screen(Rankings, Pid);
        {Pid, speed} ->
            Pid ! {self(), speed, Speed},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared);
        _Msg ->
            % GameRoom ! {unexpected_msg, self(), Msg, Pid},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed,
                          NumCleared)
    end.



new_players([]) -> [];
new_players([{Name, Pid, List} | T]) -> 
    [{Name, Pid, List, true} | new_players(T)].

% PlayerNum start at 0 (first other player)
get_player_win(PlayerNum, {Y, X, _, H}) ->
    W = ?BOARD_WIDTH * 2,
    {Y, X + W + 2 + (PlayerNum * W), ?BOARD_WIDTH, H}.

draw_boards(PlayersList, {Y, X, Width, Height}, GameRoom) ->
    lists:foldl(fun ({Player, Board}, Col) ->
                    tetris_io:draw_board(Board, {Y, Col, Width, Height}),
                    GameRoom ! {player, Player, col, Col},
                    Col + (?BOARD_WIDTH * 2)
                end, X + (?BOARD_WIDTH * 2) + 2, PlayersList).

get_board(Players, PlayerInfo) -> get_board_r(Players, PlayerInfo, 0).


get_board_r([], _PlayerInfo, _Idx) -> {error, doesnotexist};
get_board_r([{{_, PlayerPid, _, _}, Board} | _T], PlayerPid, Idx) -> 
    {Idx, Board};
get_board_r([_ | T], PlayerPid, Idx) ->
    get_board_r(T, PlayerPid, Idx + 1).

update_board([], _PlayerPid, _NewBoard) -> [];
update_board([{PlayerInfo={_, PlayerPid, _, _}, _Board} | T],
             PlayerPid, NewBoard) -> 
    [{PlayerInfo, NewBoard} | T];
update_board([H | T], PlayerInfo, NewBoard) ->
    [H | update_board(T, PlayerInfo, NewBoard)].

clear_all_boards([], _Rows) -> [];
clear_all_boards([{Player={_, _, _, true}, Board} | T], Rows) ->
    [{Player, tetris:clear_board_rows(Rows, Board)} |
     clear_all_boards(T, Rows)];
clear_all_boards([{Player={_, _, _, false}, Board} | T], Rows) ->
    [{Player, clear_rand_board_rows(Rows, Board)} | clear_all_boards(T, Rows)].



draw_player_names([], _, _) -> ok;
draw_player_names([{{Name, _, _, _}, _Board} | PlayerTail], Win, Idx) ->
    PlayerWin = get_player_win(Idx, Win),
    tetris_io:draw_centered_message(?BOARD_HEIGHT, PlayerWin, [Name]),
    draw_player_names(PlayerTail, Win, Idx + 1).

player_lost([], _) -> [];
player_lost([{{Name, Pid, List, _}, Board} | T], Pid) ->
    [{{Name, Pid, List, false}, Board} | player_lost(T, Pid)];
player_lost([H | T], Pid) ->
    [H | player_lost(T, Pid)].



print_end_game_screen(Rankings, Pid) -> 
    tetris_io:draw_end_game_screen(Rankings),
    
    receive
        {lebron, jamie} -> ok;
        {_RefreshPid, resize} -> print_end_game_screen(Rankings, Pid)
    end.

clear_rand_board_rows(Rows, Board) ->
    lists:foldl(fun (R, CurrBoard) -> board:remove_rand_row(CurrBoard, R) end,
                Board, Rows).

calc_speed(Speed, NumCleared, RowsCleared) when Speed =< 100 ->
    {Speed, NumCleared + RowsCleared};
calc_speed(Speed, NumCleared, RowsCleared)
        when (NumCleared + RowsCleared) >= 7 ->
    {Speed - 100, (NumCleared + RowsCleared) rem 7};
calc_speed(Speed, NumCleared, RowsCleared) ->
    {Speed, NumCleared + RowsCleared}.


