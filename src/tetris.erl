-module(tetris).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([initiate/1, timer/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHANGES THAT AMELIA AND TREVOR MADE:
%%% 
%%% We added a board data structure, which is a 2D array of tuples:
%%% 
%%%     {Boolean, Color}
%%% 
%%% Where Boolean indicates if a placed block is in that cell, and Color is the 
%%% color of that cell. 
%%% 
%%% We changed everything that uses coordinates so that everything that (will 
%%% eventually) live in the io file multiplies column coordinates by 2. That 
%%% way, the indexing of cells is normal in the client and server, and the IO 
%%% module is the only module that knows that cells are actually 2 wide. The 
%%% macros have been changed accordingly, and the Board data structure is 
%%% indexed like this. 
%%% 
%%% We also generate piece at the top of the board. We also changed it so that 
%%% the piece coordinates are relative to the board, so all drawing functions 
%%% have the Window passed in. 
%%% 
%%% Next steps:
%%%     1. Add getters for elements in the board data structure (maybe make a
%%%         new module?) (DONE)
%%%     2. Pass in board to delete_tetromino so we can get the correct 
%%%         background color (DONE)
%%%     3. Add bounds checking for the board (DONE)
%%% 
%%% New Next Steps:
%%%     0. Add board abstractions
%%%     1. Add block placing (bottom of the board)
%%%     2. Add bounds checking for other blocks
%%%     3. Add falling
%%%     4. Add block placing (on other blocks)
%%% 
%%% 


initiate(UserName) ->
    {_KeyPid, _MaxRow, _MaxCol} = tetris_io:init(),
    Status = start_screen(UserName),
    tetris(Status).

tetris(quit) ->
    tetris_io:stop();
% tetris(tboxtest) ->
%     Win = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
%     Input = tetris_io:text_box(Win, 15, 10),
%     io:format("~s", [Input]),
%     timer:sleep(2000),
%     tetris_io:stop();
% % tetris({multi, GameRoom}) ->
tetris({game, GameRoom}) ->
    wait_to_start(),
    start_game(GameRoom).

start_game(GameRoom) ->
    Win = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
    Board = board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, ?BACKGROUND_COLOR),
    {T, TimerPid} = tetromino:generate(self(), 1000, {1, 5}, GameRoom),
    tetris_io:draw_board(Board, Win),
    tetris_io:draw_tetromino(T, Win),
    Ghost = get_ghost(T, Board),
    tetris_io:delete_tetromino(Ghost, Win, Board),
    tetris_io:draw_ghost(Ghost, Win, Board),
    wait_for_input(T, Ghost, Win, Board, TimerPid, GameRoom).

wait_to_start() ->
    receive
        {Pid, start} -> Pid;
        _ -> wait_to_start()
    end.

title_screen(Win, UserName) ->
    tetris_io:draw_title_screen(Win, ?TITLE_MSG),
    Status = title_screen_keyboard_loop(UserName, Win),
    tetris_io:paint_screen(scrbg),
    Status.

get_num_players() -> 
    receive
        {_Pid, key, Key} when Key =< $5, Key >= $2 -> 
            Key - $0;
        {_Pid, key, $q} -> 
            0;
        {_Pid, key, _} -> get_num_players()
    end.

create_multi_room(UserName, Win) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    tetris_io:draw_title_screen(Win, ["Enter the number of players (2-5)"]),
    NumPlayers = get_num_players(),
    case NumPlayers of
        0 -> title_screen_keyboard_loop(UserName, Win);
        _ ->  GameRoom = server:create_room('server@vm-hw09.eecs.tufts.edu', RoomName, UserName, NumPlayers),
            case GameRoom of 
                already_exists -> 
                    Msg = lists:append(["Room ", RoomName, " already exists!"]),
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win);
                _ -> {game, GameRoom}
            end
    end.

join_multi_room(UserName, Win) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    GameRoom = server:join_room('server@vm-hw09.eecs.tufts.edu', RoomName, UserName),
    case GameRoom of 
        room_full -> 
            Msg = lists:append(["Room ", RoomName, " is full, sorry :("]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win);
        no_such_room -> 
            Msg = lists:append(["Room ", RoomName, " does not exist!"]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win);
        _ -> {game, GameRoom}
    end.

error_title(Win, Msg) -> 
    tetris_io:draw_title_screen(Win, [Msg, "Press [Space] to continue"]),
    receive_space(),
    tetris_io:draw_title_screen(Win, ?TITLE_MSG).

receive_space() -> 
    receive
        {_Pid, key, ?KEY_SPACE}-> ok;
        _ -> receive_space()
    end.

title_screen_keyboard_loop(UserName, Win) ->
    receive
        {_Pid, key, $1} -> 
            GameRoom = server:create_room('server@vm-hw09.eecs.tufts.edu', UserName, UserName, 1),
            case GameRoom of 
                already_exists -> 
                    Msg = "You are already playing a solo game! :P",
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win);
                _ -> {game, GameRoom}
            end;
        {_Pid, key, $2} -> 
            % io:format("messaging server...~n"),
            create_multi_room(UserName, Win);
        {_Pid, key, $3} -> 
            join_multi_room(UserName, Win);
        {_Pid, key, $q} -> quit;
        % {_Pid, key, $t} -> tboxtest;
        {_Pid, key, _} -> title_screen_keyboard_loop(UserName, Win)
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
    GameRoom ! {rowcleared, Rows, self()},
    ok.

clear_row(Rows, Board, Win) ->
    NewBoard = lists:foldl(fun (R, CurrBoard) -> board:remove_row(CurrBoard, R) end, Board, Rows),
    tetris_io:animate_clear_row(Rows, Win, 0),
    tetris_io:draw_board(NewBoard, Win),
    NewBoard.

get_ghost(Tetromino, Board) ->
    tetromino:move_to_lowest_position(Tetromino, Board).

process_key(Key, Tetromino, Ghost, Win, Board, TimerPid, GameRoom) -> 
    {ResultWin, ResultTetromino} = case Key of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        ?ceKEY_UP ->
            {Win, tetromino:rotate(1, Tetromino)};
        ?ceKEY_LEFT ->
            {Win, tetromino:move_left(Tetromino)};
        ?ceKEY_RIGHT ->
            {Win, tetromino:move_right(Tetromino)};
        ?ceKEY_DOWN ->
            {Win, tetromino:move_down(Tetromino)};
        ?KEY_SPACE ->
            {Win, tetromino:move_to_lowest_position(Tetromino, Board)};
        $z -> 
            {Win, tetromino:rotate(-1, Tetromino)};
        ?KEY_RESIZE ->
            NewWin = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
            {NewWin, Tetromino};
        _ -> 
            {Win, Tetromino}
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
                            check_clear_row(Tetromino, NewBoard, GameRoom),
                            {NewTetromino, NewTimerPid} = tetromino:generate(self(), 1000, {1, 5}, GameRoom),
                            NewGhost = get_ghost(NewTetromino, NewBoard),
                            tetris_io:draw_ghost(NewGhost, Win, NewBoard),
                            tetris_io:delete_tetromino(Tetromino, Win, NewBoard),
                            tetris_io:draw_tetromino(NewTetromino, ResultWin),
                            {Win, NewTetromino, NewGhost, NewBoard, NewTimerPid};
                        _ -> {ResultWin, Tetromino, Ghost, Board, TimerPid}
                    end
            end;
        false ->  % Redraw new tetromino and ghost
            case Key of 
                ?KEY_SPACE -> 
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    process_key(?ceKEY_DOWN, NewT2, Ghost, Win, Board, TimerPid, GameRoom);
                _ -> 
                    ResultGhost = get_ghost(NewT2, Board),
                    tetris_io:delete_tetromino(Ghost, Win, Board),
                    tetris_io:draw_ghost(ResultGhost, Win, Board),
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    tetris_io:draw_tetromino(NewT2, ResultWin),
                    {ResultWin, NewT2, ResultGhost, Board, TimerPid}
            end
    end.

wait_for_input(Tetromino, Ghost, Win, Board, TimerPid, GameRoom) ->
    % io:format("Timer: ~p~n", [TimerPid]),
    receive
        {TimerPid, timer} ->
            case process_key(?ceKEY_DOWN, Tetromino, Ghost, Win, Board, TimerPid, GameRoom) of
                blocked -> ok;
                {NewWin, NewTetromino, NewGhost, NewBoard, NewTimerPid} ->
                           wait_for_input(NewTetromino, NewGhost, NewWin, NewBoard, NewTimerPid, GameRoom)
            end;
        {_Pid, key, $q} -> 
            tetris_io:stop(),
            io:format("Thanks for playing!~n");
        {_Pid, key, Key} ->
            case process_key(Key, Tetromino, Ghost, Win, Board, TimerPid, GameRoom) of
                blocked -> ok;
                {NewWin, NewTetromino, NewGhost, NewBoard, NewTimerPid} ->
                    wait_for_input(NewTetromino, NewGhost, NewWin, NewBoard, NewTimerPid, GameRoom)
            end;
        {clearrow, Rows} -> 
            io:format("row cleared~n"),
            NewBoard = clear_row(Rows, Board, Win),
            wait_for_input(Tetromino, Ghost, Win, NewBoard, TimerPid, GameRoom);
        {newpiece, _PInfo, _T} -> 
            % io:format("Player ~p got a new piece!", [PInfo]),
            % send message to pid that controls other player's boards???
            wait_for_input(Tetromino, Ghost, Win, Board, TimerPid, GameRoom);
        {placepiece, _PInfo, _T} ->
            wait_for_input(Tetromino, Ghost, Win, Board, TimerPid, GameRoom);
        Other ->
            io:format("Client Unexpected msg: ~p~n", [Other])
    end.

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
    
start_screen(UserName) ->
    TitleWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
    title_screen(TitleWin, UserName).

