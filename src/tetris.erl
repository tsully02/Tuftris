-module(tetris).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([initiate/0]).

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


initiate() ->
    {_KeyPid, _MaxRow, _MaxCol} = tetris_io:init(),
    Status = start_screen(),
    tetris(Status).

tetris(quit) ->
    tetris_io:stop();
tetris(start) ->
    Win = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
    Board = board:create_board(?BOARD_WIDTH, ?BOARD_HEIGHT, ?BACKGROUND_COLOR),
    {T, TimerPid} = generate_tetromino(self(), 1000, {1, 5}),
    tetris_io:draw_board(Board, Win),
    tetris_io:draw_tetromino(T, Win),
    Ghost = get_ghost(T, Board),
    tetris_io:delete_tetromino(Ghost, Win, Board),
    tetris_io:draw_ghost(Ghost, Win, Board),
    wait_for_input(T, Ghost, Win, Board, TimerPid).

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

generate_tetromino(Pid, Time, {Row, Col}) ->
    Tetrominos = [t, line, zigz, zags, square, left, right],
    % random:seed(erlang:now()),
    Type = lists:nth(rand:uniform(7), Tetrominos),
    TimerPid = spawn(fun () -> timer(Pid, Time) end),
    % io:format("Timer: ~p~n", [TimerPid]),
    {{Type, 0, {Row, Col}, get_rot(Type, 0)}, TimerPid}.

% rotates a tetromino
% If Direction = 1, rotates clockwise, if -1, rotates counter clockwise
% 
% Each tetromino piece will have a hard coded list of Cells: each direciton 
% has a different set of cells. Since the cells are relative to the center, we 
% can simply replace the cells in a tetromino tuple to rotate.
% 
% 
rotate_tetromino(Direction, {Type, Rotation, {CenterRow, CenterCol}, _Cells}) ->
    % erlang does not support negative modding, so we add 4 here to always 
    % ensure that it's positive
    NewR = (Rotation + Direction + 4) rem 4,
    NewCells = get_rot(Type, NewR),
    NewCenter = case Type of 
                    line ->
                    case Rotation of
                        0 when Direction == -1-> 
                            change_line_center({CenterRow, CenterCol}, 0);
                        1 when Direction == 1-> 
                            change_line_center({CenterRow, CenterCol}, 0);
                        2 when Direction == -1-> 
                            change_line_center({CenterRow, CenterCol}, 1);
                        3 when Direction == 1-> 
                            change_line_center({CenterRow, CenterCol}, 1);
                        _ -> {CenterRow, CenterCol}
                    end;
                    _ -> {CenterRow, CenterCol}
    end,
    {Type, NewR, NewCenter, NewCells}.

change_line_center({CenterRow, CenterCol}, ChangeIndex) ->
    List = ?Rotation_Line_Centers,
    {ChangeRow, ChangeCol} = lists:nth(ChangeIndex + 1, List),
    {CenterRow + ChangeRow, CenterCol + ChangeCol}.

get_rot(Type, Rotation) ->
    case Type of 
        t -> List = ?Rotation_T;
        left -> List = ?Rotation_Left;
        right -> List = ?Rotation_Right;
        square -> List = ?Rotation_Square;
        zigz -> List = ?Rotation_Zigz;
        zags -> List = ?Rotation_Zags;
        line -> List = ?Rotation_Line
    end,
    lists:nth(Rotation + 1, List).



% Take a tetromino and adjust its bounds to keep it in bounds, then return the new tetromino
check_tetromino_bounds({Type, Rotation, {CRow, CCol}, Cells}) ->
    Tet = {Type, Rotation, {CRow, CCol}, Cells},
    PredL = fun ({_Row, Col}) -> Col < 0 end,
    PredR = fun ({_Row, Col}) -> Col > ?BOARD_WIDTH - 1 end,
    Fun = case lists:map(fun (Pred) -> lists:any(Pred, tetromino:get_all_coords(Tet)) end, [PredL, PredR]) of
        [true, _] -> fun (T) -> check_tetromino_bounds(move_right(T)) end;
        [_, true] -> fun (T) -> check_tetromino_bounds(move_left(T)) end;
        _ -> fun (T) -> T end
    end,
    Fun(Tet).

% Return whether a tetromino is overlapping any filled cells on the board or against the bottom
check_tetromino_collision({Type, Rotation, {CRow, CCol}, Cells}, Board) ->
    T = {Type, Rotation, {CRow, CCol}, Cells},
    TCoords = tetromino:get_all_coords(T),
    BottomPred = fun ({Row, _Col}) -> Row > ?BOARD_HEIGHT - 1 end,
    OnBottom = lists:any(BottomPred, TCoords),
    OnBottom orelse lists:any(fun ({Row, Col}) -> board:is_filled(Board, Row, Col) end, TCoords).

clear_row({Type, Rotation, Center, Cells}, Board, Win) ->
    % for each cell that was placed, check the row
    Placed = tetromino:get_all_coords({Type, Rotation, Center, Cells}),
    Sorted = lists:sort(fun ({R1, _}, {R2, _}) -> R1 < R2 end, Placed),
    % file:write_file("output.txt", io_lib:fwrite("piece ~p has been Sorted in cells ~p~n", [Type, Sorted]), [append]),
    {FinalBoard, Rows} = lists:foldl(
        fun ({Row, _Col}, {CurrBoard, Rows}) ->
            % io:format("Row, Col: ~p, ~p~n", [Row, Col]),
            % file:write_file("output.txt", io_lib:fwrite("Row, Col: ~p, ~p~n", [Row, Col]), [append]),
            case lists:all(fun (CurrCol) -> board:is_filled(CurrBoard, Row, CurrCol) end, lists:seq(0, ?BOARD_WIDTH - 1))
            of
                true -> %file:write_file("output.txt", io_lib:fwrite("removing row ~p~n", [Row]), [append]),
                        % tetris_io:animate_clear_row(Row, Win, 0),
                        NewBoard = board:remove_row(CurrBoard, Row),
                        % tetris_io:draw_board(NewBoard, Win),
                        {NewBoard, [Row | Rows]};
                false -> {CurrBoard, Rows}
            end
        end,
        {Board, []},
        Sorted),
    tetris_io:animate_clear_row(Rows, Win, 0),
    tetris_io:draw_board(FinalBoard, Win),
    FinalBoard.

move_left({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol - 1}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_down({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow + 1, CenterCol}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_up({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow - 1, CenterCol}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_right({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol + 1}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

% move tetromino to specified coordinate (will be used for implementing space)
% move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) when Row < 0 ->
%     {Type, Rotation, {0, Col}, Cells};
% move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) when Col < 0 ->
%     {Type, Rotation, {Row, 0}, Cells};
move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) ->
    {Type, Rotation, {Row, Col}, Cells}.

move_tetromino_to_lowest_position(Tetromino, Board) ->
    DownTetromino = move_down(Tetromino),
    case check_tetromino_collision(DownTetromino, Board) of
        true -> Tetromino;
        false -> move_tetromino_to_lowest_position(DownTetromino, Board)
    end.

get_ghost(Tetromino, Board) ->
    move_tetromino_to_lowest_position(Tetromino, Board).

process_key(Key, Tetromino, Ghost, Win, Board, TimerPid) -> 
    {ResultWin, ResultTetromino} = case Key of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        ?ceKEY_UP ->
            {Win, rotate_tetromino(1, Tetromino)};
        ?ceKEY_LEFT ->
            {Win, move_left(Tetromino)};
        ?ceKEY_RIGHT ->
            {Win, move_right(Tetromino)};
        ?ceKEY_DOWN ->
            {Win, move_down(Tetromino)};
        32 ->
            {Win, move_tetromino_to_lowest_position(Tetromino, Board)};
        $z -> 
            {Win, rotate_tetromino(-1, Tetromino)};
        ?KEY_RESIZE ->
            NewWin = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
            {NewWin, Tetromino};
        _ -> 
            {Win, Tetromino}
    end,
    NewT2 = check_tetromino_bounds(ResultTetromino),
    case check_tetromino_collision(NewT2, Board) of
        true ->  % Reject move, generate new tetromino if down
            case Key of 
                ?ceKEY_DOWN -> NewBoard = board:place_piece(Board, Tetromino),
                               tetris_io:draw_board(NewBoard, Win),
                               TimerPid ! {self(), kill},
                               NewNewBoard = clear_row(Tetromino, NewBoard, Win),
                               {NewTetromino, NewTimerPid} = generate_tetromino(self(), 1000, {1, 5}),
                               NewGhost = get_ghost(NewTetromino, NewNewBoard),
                               tetris_io:draw_ghost(NewGhost, Win, NewNewBoard),
                               tetris_io:delete_tetromino(Tetromino, Win, NewNewBoard),
                               tetris_io:draw_tetromino(NewTetromino, ResultWin),
                               {Win, NewTetromino, NewGhost, NewNewBoard, NewTimerPid};
                _ -> {ResultWin, Tetromino, Ghost, Board, TimerPid}
            end;
        false ->  % Redraw new tetromino and ghost
            case Key of 
                32 -> 
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    process_key(?ceKEY_DOWN, NewT2, Ghost, Win, Board, TimerPid);
                _ -> 
                    ResultGhost = get_ghost(NewT2, Board),
                    tetris_io:delete_tetromino(Ghost, Win, Board),
                    tetris_io:draw_ghost(ResultGhost, Win, Board),
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    tetris_io:draw_tetromino(NewT2, ResultWin),
                    {ResultWin, NewT2, ResultGhost, Board, TimerPid}
            end
    end.

wait_for_input(Tetromino, Ghost, Win, Board, TimerPid) ->
    % io:format("Timer: ~p~n", [TimerPid]),
    receive
        {TimerPid, timer} ->
            {NewWin, NewTetromino, NewGhost, NewBoard, NewTimerPid} = process_key(?ceKEY_DOWN, Tetromino, Ghost, Win, Board, TimerPid),
            wait_for_input(NewTetromino, NewGhost, NewWin, NewBoard, NewTimerPid);
        {_Pid, key, $q} -> 
            tetris_io:stop(),
            io:format("Thanks for playing!~n");
        {_Pid, key, Key} -> 
            {NewWin, NewTetromino, NewGhost, NewBoard, NewTimerPid} = process_key(Key, Tetromino, Ghost, Win, Board, TimerPid),
            wait_for_input(NewTetromino, NewGhost, NewWin, NewBoard, NewTimerPid);
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
    

% add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
% add_horiz_line_c(Row, Col, Length, ColorNum) ->
%     cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
%     cecho:attron(?ceCOLOR_PAIR(ColorNum)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).

% add_horiz_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
% add_horiz_line(Row, Col, Length) when Length rem 2 == 0 ->
%     cecho:attron(?ceCOLOR_PAIR(60)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line(Row, Col + 1, Length - 1);
% add_horiz_line(Row, Col, Length) when Length rem 2 == 1 ->
%     cecho:attron(?ceCOLOR_PAIR(8)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line(Row, Col + 1, Length - 1).

% add_check_horiz_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
% add_check_horiz_line(Row, Col, Length) when Length rem 2 == 0 ->
%     cecho:attron(?ceCOLOR_PAIR(60)),
%     draw_square({Row, Col}),
%     add_check_horiz_line(Row, Col + 2, Length - 1);
% add_check_horiz_line(Row, Col, Length) when Length rem 2 == 1 ->
%     cecho:attron(?ceCOLOR_PAIR(8)),
%     draw_square({Row, Col}),
%     add_check_horiz_line(Row, Col + 2, Length - 1).

% add_vert_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
% add_vert_line(Row, Col, Length) ->
%     cecho:mvaddch(Row, Col, $|),
%     add_vert_line(Row + 1, Col, Length - 1).




start_screen() ->
    TitleWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
    tetris_io:title_screen(TitleWin).

