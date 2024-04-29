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
            PlayersList = lists:map(fun (Player) -> {Player, board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, bg)} end, NewPlayers),
            % GameRoom ! {playerlist, PlayersList},
            paint_players(Self, Preview, PlayersList, Win, GameRoom, 1000, 0)
    end.

%%% paint_players/7
paint_players(Self={{Name, Pid, Listener}, Board}, Preview, Players, Win, GameRoom, Speed, NumCleared) ->
    Me = {Name, Pid, Listener},
    receive
        {lebron, jamie} -> ok; % Goated with house sauce, kid perhaps
        {Pid, draw} ->
            tetris_io:draw_board(Board, Win),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, Win, ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed, NumCleared);
        {Pid, new_board, NewBoard} -> 
            NewSelf = {Me, NewBoard},
            tetris_io:draw_board(NewBoard, Win),
            cecho:refresh(),
            paint_players(NewSelf, Preview, Players, Win, GameRoom, Speed, NumCleared);
        {GameRoom, clearplayer, PlayerPid} -> 
            RandBoard = board:create_random(?BOARD_WIDTH, ?BOARD_HEIGHT),
            {Idx, _} = get_board(Players, PlayerPid),
            % NewBoard = board:place_piece(PlayerBoard, T),
            BoardWin = get_player_win(Idx, Win),
            tetris_io:draw_board(RandBoard, BoardWin),
            NewPlayers = update_board(Players, PlayerPid, RandBoard),
            paint_players(Self, Preview, player_lost(NewPlayers, PlayerPid), Win, GameRoom, Speed, NumCleared);
        {Pid, clear_row, NewBoard, Rows, Tetromino} ->
            NewSelf = {Me, NewBoard},
            NewPlayers = clear_all_boards(Players, Rows),
            FirstWin = get_player_win(0, Win),
            tetris_io:animate_clear_row(Rows, ?CLEAR_ROW_SLEEP, Win, ?BOARD_WIDTH),
            tetris_io:draw_board(NewBoard, Win),
            Ghost = tetromino:get_ghost(Tetromino, NewBoard),
            tetris_io:draw_ghost(Ghost, Win, NewBoard),
            tetris_io:draw_tetromino(Tetromino, Win),
            cecho:refresh(),
            % GameRoom ! length(Players),
            Sleep = ?CLEAR_ROW_SLEEP div (length(Players) + 1),
            tetris_io:animate_clear_row(Rows, Sleep, FirstWin, ?BOARD_WIDTH * length(Players)),
            draw_boards(NewPlayers, Win, GameRoom),
            % tetris_io:draw_board(NewBoard, Win),
            cecho:refresh(),
            Pid ! {self(), continue},
            GameRoom ! {length, length(Rows)},
            {NewSpeed, NewNumCleared} = calc_speed(Speed, NumCleared, length(Rows)),
            paint_players(NewSelf, Preview, NewPlayers, Win, GameRoom, NewSpeed, NewNumCleared);

        {Pid, clear_other_rows, Rows} ->
            NewPlayers = clear_all_boards(Players, Rows),
            FirstWin = get_player_win(0, Win),
            Sleep = ?CLEAR_ROW_SLEEP div (length(Players) + 1),
            tetris_io:animate_clear_row(Rows, Sleep, FirstWin, ?BOARD_WIDTH * length(Players)),
            draw_boards(NewPlayers, Win, GameRoom),
            cecho:refresh(),
            paint_players(Self, Preview, NewPlayers, Win, GameRoom, Speed, NumCleared);

        {Pid, draw_tetromino, OldT, NewT} ->
            OldG = tetromino:get_ghost(OldT, Board),
            tetris_io:delete_tetromino(OldG, Win, Board),
            tetris_io:delete_tetromino(OldT, Win, Board),
            NewG = tetromino:get_ghost(NewT, Board),
            tetris_io:draw_ghost(NewG, Win, Board),
            tetris_io:draw_tetromino(NewT, Win),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed, NumCleared);

        {Pid, draw_preview, NewPreview} ->
            tetris_io:draw_preview(NewPreview, Win, bg),
            paint_players(Self, NewPreview, Players, Win, GameRoom, Speed, NumCleared);
        draw -> 
            tetris_io:draw_board(Board, Win),
            draw_boards(Players, Win, GameRoom),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, Win, ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, Win, GameRoom, Speed, NumCleared);
        {GameRoom, placepiece, PlayerPid, T} -> 
            {Idx, PlayerBoard} = get_board(Players, PlayerPid),
            NewBoard = board:place_piece(PlayerBoard, T),
            BoardWin = get_player_win(Idx, Win),
            tetris_io:draw_board(NewBoard, BoardWin),
            cecho:refresh(),
            NewPlayers = update_board(Players, PlayerPid, NewBoard),
            paint_players(Self, Preview, NewPlayers, Win, GameRoom, Speed, NumCleared);
        {_RefreshPid, resize} ->
            NewWin = tetris_io:calc_win_coords(tetris:get_game_width(length(Players) + 1), ?BOARD_HEIGHT),
            tetris_io:paint_screen(border),
            tetris_io:draw_preview(Preview, NewWin, bg),
            tetris_io:draw_board(Board, NewWin),
            draw_boards(Players, NewWin, GameRoom),
            tetris_io:set_color(border),
            draw_player_names(Players, Win, 0),
            tetris_io:draw_centered_message(2 + ?BOARD_HEIGHT, NewWin, ?KEYBINDS),
            cecho:refresh(),
            paint_players(Self, Preview, Players, NewWin, GameRoom, Speed, NumCleared);
        {Pid, username} ->
            Pid ! {self(), user, Name},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed, NumCleared);
        {Pid, end_game, Rankings} ->
            % io:format("HOWDY~n"),
            print_end_game_screen(Rankings, Pid);
        {Pid, speed} ->
            Pid ! {self(), speed, Speed},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed, NumCleared);
        _Msg ->
            % GameRoom ! {unexpected_msg, self(), Msg, Pid},
            paint_players(Self, Preview, Players, Win, GameRoom, Speed, NumCleared)
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
update_board([{PlayerInfo={_, PlayerPid, _, _}, _Board} | T], PlayerPid, NewBoard) -> 
    [{PlayerInfo, NewBoard} | T];
update_board([H | T], PlayerInfo, NewBoard) ->
    [H | update_board(T, PlayerInfo, NewBoard)].

clear_all_boards([], _Rows) -> [];
clear_all_boards([{Player={_, _, _, true}, Board} | T], Rows) ->
    [{Player, tetris:clear_board_rows(Rows, Board)} | clear_all_boards(T, Rows)];
clear_all_boards([{Player={_, _, _, false}, Board} | T], Rows) ->
    [{Player, clear_rand_board_rows(Rows, Board)} | clear_all_boards(T, Rows)].

calc_speed(Speed, NumCleared, RowsCleared) when Speed =< 100 ->
    {Speed, NumCleared + RowsCleared};
calc_speed(Speed, NumCleared, RowsCleared) when (NumCleared + RowsCleared) >= 7 ->
    {Speed - 100, (NumCleared + RowsCleared) rem 7};
calc_speed(Speed, NumCleared, RowsCleared) ->
    {Speed, NumCleared + RowsCleared}.

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
    cecho:refresh(),
    tetris_io:paint_screen(border),
    NewWin = tetris_io:calc_win_coords(10, ?BOARD_HEIGHT),
    {WinR, WinC, _, _} = NewWin,    
    tetris_io:set_color(bg),
    RankHeight = length(Rankings) + 1,
    tetris_io:paint_box({WinR, WinC}, 19, RankHeight),
    tetris_io:set_color(ghost),
    cecho:mvaddstr(WinR, WinC, "Rankings:"),
    print_rankings(Rankings, {WinR + 1, WinC}, 0),

    tetris_io:set_color(bg),
    {_, PodiumC, PodiumW, PodiumH} = tetris_io:calc_win_coords(16, WinR + RankHeight + 1 + 10),
    PodiumBoxCoords = {PodiumR=WinR + RankHeight + 1, PodiumC},
    tetris_io:paint_box(PodiumBoxCoords, 29, 10),

    draw_podiums(Rankings, {PodiumR, PodiumC}, 0),

    % {SilverR, SilverC} = {PodiumR + 6, PodiumC + 1},
    % tetris_io:set_color(silver),
    % tetris_io:paint_box({SilverR, SilverC}, 9, 4),
    % SGuy = {SGuyR, SGuyC, SGuyW, SGuyH} = {SilverR - 3, SilverC, 5, 3},
    % tetris_io:set_color(ghost),
    % tetris_io:draw_centered_message(0, {SGuyR, SGuyC, SGuyW, SGuyH}, ["  o7 ", " /|  ", " / \\ "]),

    % {GoldR, GoldC} = {PodiumR + 5, PodiumC + 10},
    % tetris_io:set_color(gold),
    % tetris_io:paint_box({PodiumR + 5, PodiumC + 10}, 9, 5),
    % GGuy = {GGuyR, GGuyC, GGuyW, GGuyH} = {GoldR - 3, GoldC, 5, 3},
    % tetris_io:set_color(ghost),
    % tetris_io:draw_centered_message(0, {GGuyR, GGuyC, GGuyW, GGuyH}, [" \\o/ ", "  |  ", " / \\ "]),

    % {BronzeR, BronzeC} = {PodiumR + 7, PodiumC + 19},
    % tetris_io:set_color(bronze),
    % tetris_io:paint_box({PodiumR + 7, PodiumC + 19}, 9, 3),
    % BGuy = {BGuyR, BGuyC, BGuyW, BGuyH} = {BronzeR - 3, BronzeC, 5, 3},
    % tetris_io:set_color(ghost),
    % tetris_io:draw_centered_message(0, {BGuyR, BGuyC, BGuyW, BGuyH}, ["  o  ", " /|\\ ", " / \\ "]),
    

    tetris_io:set_color(border),
    % tetris_io:draw_centered_message(0, {WinR + RankHeight + 2 + 10, PodiumC, PodiumW, PodiumH}, ["Press [Space] to continue"]),
    tetris_io:draw_centered_message(14, NewWin, ["Press [Space] to continue"]),
    cecho:refresh(),
    receive
        {lebron, jamie} -> ok;
        {_RefreshPid, resize} -> print_end_game_screen(Rankings, Pid)
    end.

draw_podiums(_, _, 3) -> ok;
draw_podiums([], _, _) -> ok;
draw_podiums([{Name, _Pid} | T], {P_R, P_C}, Idx) -> 
    {{PodiumR, PodiumC}, Color, Msg} = case Idx of 
        0 -> {{P_R + 5, P_C + 10}, gold, [" \\o/ ", "  |  ", " / \\ "]}; 
        1 -> {{P_R + 6, P_C + 1}, silver, ["  o7 ", " /|  ", " / \\ "]}; 
        2 -> {{P_R + 7, P_C + 19}, bronze, ["  o  ", " /|\\ ", " / \\ "]}
    end, 
    tetris_io:set_color(Color),
    tetris_io:paint_box({PodiumR, PodiumC}, 9, 5 - Idx),
    tetris_io:draw_centered_message(4 - Idx, {PodiumR, PodiumC, 4, 5}, [Name]),
    Guy = {PodiumR - 3, PodiumC, 5, 3},
    tetris_io:set_color(ghost),
    tetris_io:draw_centered_message(0, Guy, Msg),
    draw_podiums(T, {P_R, P_C}, Idx + 1).


print_rankings([], _, _) -> ok;
print_rankings([{Name, _Pid} | T], Win={R, C}, Idx) ->
    case Idx of
        0 -> tetris_io:set_color(gold_text);
        1 -> tetris_io:set_color(silver_text);
        2 -> tetris_io:set_color(bronze_text);
        _ -> tetris_io:set_color(ghost)
    end, 
    Str = " " ++ integer_to_list(Idx + 1) ++ ". " ++ Name,
    cecho:mvaddstr(R, C, Str),
    print_rankings(T, {R + 1, C}, Idx + 1).

clear_rand_board_rows(Rows, Board) ->
    lists:foldl(fun (R, CurrBoard) -> board:remove_rand_row(CurrBoard, R) end, Board, Rows).


