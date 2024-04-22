-module(tetris).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([initiate/1, timer/2]).

initiate(UserName) ->
    Listener = spawn_link(fun () -> listener(UserName) end),
    {_KeyPid, RefreshPid, _MaxRow, _MaxCol} = tetris_io:init(),
    % Num = add_horiz_line_c(30, 30, 11000, 1000),
    % io:format("~p~n", [Num]),
    % cecho:refresh(),
    % receive_space("ope"),
    Status = start_screen(UserName, Listener),
    % Disable auto refresh during the game itself; leave it up to the main client process
    tetris_io:set_auto_refresh(RefreshPid, false),
    tetris(Status, Listener).

% add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
% add_horiz_line_c(Row, Col, Length, ColorNum) ->
%     cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
%     cecho:attron(?ceCOLOR_PAIR(ColorNum)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).

tetris(quit, _) ->
    tetris_io:stop();
tetris({game, {GameRoom, NumPlayers}}, Listener) ->
    Players = wait_to_start([]),
    start_game(GameRoom, NumPlayers, Listener).

start_game(GameRoom, NumPlayers, Listener) ->
    % io:format("LENGTH: ~p~n", [length(Players)]),
    % timer:sleep(2000),
    Win = tetris_io:calc_game_win_coords(?BOARD_WIDTH * NumPlayers + 2, ?BOARD_HEIGHT),
    Listener ! {self(), window, Win},
    Board = board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, ?BACKGROUND_COLOR),
    TimerPid = new_timer(self(), 1000),
    T = tetromino:generate({1, 5}, GameRoom),
    GameRoom ! {newpiece, T, self()},
    tetris_io:paint_screen(border),
    tetris_io:draw_board(Board, Win),
    % tetris_io:draw_board(Board, Win),
    tetris_io:draw_tetromino(T, Win),
    Ghost = get_ghost(T, Board),
    Preview = [tetromino:generate({1, 5}, GameRoom), tetromino:generate({1, 5}, GameRoom), tetromino:generate({1, 5}, GameRoom)],
    tetris_io:draw_preview(Preview, Win, bg),
    tetris_io:delete_tetromino(Ghost, Win, Board),
    tetris_io:draw_ghost(Ghost, Win, Board),
    wait_for_input(T, Ghost, Preview, Win, Board, TimerPid, GameRoom).

new_timer(Pid, Time) ->
    spawn(fun () -> timer(Pid, Time) end).

wait_to_start(Players) ->
    receive
        {_Pid, players, P} -> wait_to_start(P);
        {_Pid, start} -> Players;
        _ -> wait_to_start(Players)
    end.

title_screen(Win, UserName, Listener) ->
    tetris_io:draw_title_screen(Win, ?TITLE_MSG),
    Status = title_screen_keyboard_loop(UserName, Win, Listener),
    tetris_io:paint_screen(scrbg),
    Status.

get_num_players(Msg) -> 
    receive
        {_Pid, key, Key} when Key =< $5, Key >= $2 -> 
            Key - $0;
        {_Pid, key, $q} -> 
            0;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            get_num_players(Msg);
        {_Pid, key, _} -> get_num_players(Msg)
    end.

create_multi_room(UserName, Win, Listener) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    NumPlayersMsg = ["Enter the number of players (2-5)"],
    tetris_io:draw_title_screen(Win, NumPlayersMsg),
    NumPlayers = get_num_players(NumPlayersMsg),
    case NumPlayers of
        0 -> title_screen_keyboard_loop(UserName, Win, Listener);
        _ ->  GameRoom = server:create_room('t@vm-hw02.eecs.tufts.edu', RoomName, UserName, NumPlayers, Listener),
            case GameRoom of 
                already_exists -> 
                    Msg = lists:append(["Room ", RoomName, " already exists!"]),
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win, Listener);
                _ -> {game, {GameRoom, NumPlayers}}
            end
    end.

join_multi_room(UserName, Win, Listener) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    GameInfo = server:join_room('t@vm-hw02.eecs.tufts.edu', RoomName, UserName, Listener),
    case GameInfo of 
        room_full -> 
            Msg = lists:append(["Room ", RoomName, " is full, sorry :("]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win, Listener);
        no_such_room -> 
            Msg = lists:append(["Room ", RoomName, " does not exist!"]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win, Listener);
        _ -> {game, GameInfo}
    end.

error_title(Win, Msg) ->
    FullMsg = [Msg, "Press [Space] to continue"],
    tetris_io:draw_title_screen(Win, FullMsg),
    receive_space(FullMsg),
    tetris_io:draw_title_screen(Win, ?TITLE_MSG).

receive_space(Msg) -> 
    receive
        {_Pid, key, ?KEY_SPACE}-> ok;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            receive_space(Msg);
        _ -> receive_space(Msg)
    end.

title_screen_keyboard_loop(UserName, Win, Listener) ->
    receive
        {_Pid, key, $1} -> 
            GameRoom = server:create_room('t@vm-hw02.eecs.tufts.edu', UserName, UserName, 1, Listener),
            case GameRoom of 
                already_exists -> 
                    Msg = "You are already playing a solo game! :P",
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win, Listener);
                _ -> {game, {GameRoom, 1}}
            end;
        {_Pid, key, $2} -> 
            % io:format("messaging server...~n"),
            create_multi_room(UserName, Win, Listener);
        {_Pid, key, $3} -> 
            join_multi_room(UserName, Win, Listener);
        {_Pid, key, $q} -> quit;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, ?TITLE_MSG),
            title_screen_keyboard_loop(UserName, NewWin, Listener);
        % {_Pid, key, $t} -> tboxtest;
        {_Pid, key, _} -> title_screen_keyboard_loop(UserName, Win, Listener)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Piece Tuple Notation:
%%% 
%%%     {Type, Rotation, Center, Cells}
%%% 
%%% Type: atom denoting the type of piece (t, square, left, right, zig, zag, line)
%%% Rotation: integer between 0 and 3 denoting the direction (0 is original 
%%%            direction, 1 is 90° clockwise, 2 is 180° clockwise, etc.)
%%% Center: "Center" index of piece (not always the actual center)
%%% Cells: Location of cells, relative to the center (add the cell values to the 
%%%             center coords to get the cell coords)
%%%
%%% Right now the program places a piece in the middle of the screen, and supports 
%%% the following keyboard inputs (see wait_for_input/1)
%%%     q: quit
%%%     Up arrow: rotate clockwise
%%%     Z: rotate counter clockwise
%%%     Right arrow: move right
%%%     Left arrow: move left
%%%     Down arrow: move down

check_clear_row({Type, Rotation, Center, Cells}, Board, GameRoom) ->
    Placed = tetromino:get_all_coords({Type, Rotation, Center, Cells}),
    Sorted = lists:sort(fun ({R1, _}, {R2, _}) -> R1 < R2 end, Placed),
    Rows = lists:foldl(
        fun ({Row, _Col}, Rows) ->
            case lists:all(fun (CurrCol) -> board:is_filled(Board, Row, CurrCol) end, lists:seq(0, ?BOARD_WIDTH - 1))
            of
                true -> [Row | Rows];
                false -> Rows
            end
        end,
        [],
        Sorted),
    case Rows of
        [] -> ok;
        _ ->
            GameRoom ! {rowcleared, Rows, self()},
            ok
    end.

clear_row(Rows, Board, Win) ->
    NewBoard = lists:foldl(fun (R, CurrBoard) -> board:remove_row(CurrBoard, R) end, Board, Rows),
    tetris_io:animate_clear_row(Rows, Win, 0),
    tetris_io:draw_board(NewBoard, Win),
    cecho:refresh(),
    NewBoard.

get_ghost(Tetromino, Board) ->
    tetromino:move_to_lowest_position(Tetromino, Board).

process_key(Key, Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom) -> 
    ResultTetromino = case Key of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        ?ceKEY_UP ->
            tetromino:rotate(1, Tetromino);
        ?ceKEY_LEFT ->
            tetromino:move_left(Tetromino);
        ?ceKEY_RIGHT ->
            tetromino:move_right(Tetromino);
        ?ceKEY_DOWN ->
            tetromino:move_down(Tetromino);
        ?KEY_SPACE ->
            tetromino:move_to_lowest_position(Tetromino, Board);
        $z -> 
            tetromino:rotate(-1, Tetromino);
        _ -> 
            Tetromino
    end,
    NewT2 = tetromino:check_bounds(ResultTetromino),
    case tetromino:check_collision(NewT2, Board) of
        true ->  % Reject move, generate new tetromino if down
            {_Type, _Rotation, Center, _Cells} = Tetromino,
            {Row, _Col} = Center,
            case Row of
                1 -> blockedOut();
                _ ->
                    case Key of 
                        ?ceKEY_DOWN -> NewBoard = board:place_piece(Board, Tetromino),
                            GameRoom ! {placepiece, Tetromino, self()},
                            tetris_io:draw_board(NewBoard, Win),
                            TimerPid ! {self(), kill},
                            NewTimerPid = new_timer(self(), 1000),
                            check_clear_row(Tetromino, NewBoard, GameRoom),
                            {NewTetromino, NewPreview} = next_piece(Preview, GameRoom),
                            tetris_io:draw_preview(NewPreview, Win, bg),
                            NewGhost = get_ghost(NewTetromino, NewBoard),
                            tetris_io:draw_ghost(NewGhost, Win, NewBoard),
                            tetris_io:delete_tetromino(Tetromino, Win, NewBoard),
                            tetris_io:draw_tetromino(NewTetromino, Win),
                            {Win, NewTetromino, NewGhost, NewPreview, NewBoard, NewTimerPid};
                        _ -> {Win, Tetromino, Ghost, Preview, Board, TimerPid}
                    end
            end;
        false ->  % Redraw new tetromino and ghost
            case Key of 
                ?KEY_SPACE -> 
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    process_key(?ceKEY_DOWN, NewT2, Ghost, Preview, Win, Board, TimerPid, GameRoom);
                _ -> 
                    ResultGhost = get_ghost(NewT2, Board),
                    tetris_io:delete_tetromino(Ghost, Win, Board),
                    tetris_io:draw_ghost(ResultGhost, Win, Board),
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    tetris_io:draw_tetromino(NewT2, Win),
                    {Win, NewT2, ResultGhost, Preview, Board, TimerPid}
            end
    end.

wait_for_input(Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom) ->
    % io:format("Timer: ~p~n", [TimerPid]),
    receive
        {TimerPid, timer} ->
            case process_key(?ceKEY_DOWN, Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom) of
                blocked -> 
                    GameRoom ! stop,
                    ok;
                {NewWin, NewTetromino, NewGhost, NewPreview, NewBoard, NewTimerPid} ->
                           wait_for_input(NewTetromino, NewGhost, NewPreview, NewWin, NewBoard, NewTimerPid, GameRoom)
            end;
        {GameRoom, gameover} ->
            tetris_io:stop(),
            io:format("Thanks for playing!~n"),
            ok;
        {_Pid, key, $q} -> 
            GameRoom ! {playerquit, self()},
            tetris_io:stop(),
            io:format("Thanks for playing!~n"),
            ok;
        {_Pid, key, $l} -> 
            NewTetromino = tetromino:generate({1, 5}, line),
            wait_for_input(NewTetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom);
        {_Pid, key, Key} ->
            case process_key(Key, Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom) of
                blocked -> 
                    GameRoom ! {playerlost, self()},
                    wait_for_input(Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom);
                {NewWin, NewTetromino, NewGhost, NewPreview, NewBoard, NewTimerPid} ->
                    wait_for_input(NewTetromino, NewGhost, NewPreview, NewWin, NewBoard, NewTimerPid, GameRoom)
            end;
        {clearrow, Rows} -> 
            % io:format("row cleared~n"),
            NewBoard = clear_row(Rows, Board, Win),
            NewGhost = get_ghost(Tetromino, NewBoard),
            tetris_io:draw_ghost(NewGhost, Win, NewBoard),
            tetris_io:draw_tetromino(Tetromino, Win),
            cecho:refresh(),
            wait_for_input(Tetromino, NewGhost, Preview, Win, NewBoard, TimerPid, GameRoom);
        {newpiece, _PInfo, _T} -> 
            % io:format("Player ~p got a new piece!", [PInfo]),
            % send message to pid that controls other player's boards???
            wait_for_input(Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom);
        {placepiece, _PInfo, _T} -> 
            wait_for_input(Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom);
        {_Pid, timer} ->
            wait_for_input(Tetromino, Ghost, Preview, Win, Board, TimerPid, GameRoom);
        {_Pid, resize} ->
                    NewWin = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
                    tetris_io:paint_screen(border),
                    tetris_io:draw_board(Board, NewWin),
                    tetris_io:draw_preview(Preview, NewWin, bg),
                    ResultGhost = get_ghost(Tetromino, Board),
                    tetris_io:draw_ghost(ResultGhost, NewWin, Board),
                    tetris_io:draw_tetromino(Tetromino, NewWin),
                    wait_for_input(Tetromino, Ghost, Preview, NewWin, Board, TimerPid, GameRoom);

        Other ->
            io:format("Client Unexpected msg: ~p~n", [Other])
    end.

next_piece([Piece | Tail], GameRoom) ->
    Ret = {Piece, lists:append([Tail, [tetromino:generate({1, 5}, GameRoom)]])},
    GameRoom ! {newpiece, Piece, self()},
    Ret.

timer(Pid, Time) ->
    timer:sleep(Time),
    receive
        {Pid, kill} -> ok
    after
        0 -> 
             Pid ! {self(), timer},
             timer(Pid, Time)
    end.

% Implement BlockedOut behavior
blockedOut() ->
    blocked.
    
start_screen(UserName, Listener) ->
    TitleWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
    title_screen(TitleWin, UserName, Listener).

listener(UserName) ->
    receive
        {GameRoom, players, Players} -> 
            Win = receive
                      {_Pid, window, Win} -> Win
                  end,
            NewPlayers = lists:keydelete(UserName, 1, Players),
            io:format("~p: ~p~n", [GameRoom, NewPlayers])
    end.



%%% 

%%% 
%%% Listener needs to:
%%% initiate map of player pids to names/boards/windows
%%%     - get message from GameRoom each time a player joins
%%% receive messages for other players' placed pieces
%%%     - then change that player's board
%%% 

% paint_players(GameRoom, Players) ->
%     receive
%         {newplayer, Player}  -> paint_players(GameRoom, [Player | Players])
%     end.