-module(tetris).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([hello_world/0]).

hello_world() -> 
    {_KeyPid, MaxRow, MaxCol} = tetris_io:init(),
    Win = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
    T = generate_tetromino(line, {?BOARD_HEIGHT div 2, ?BOARD_WIDTH div 2}),
    draw_tetromino(T, Win),
    wait_for_input(T, Win).

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

generate_tetromino(Type, {Row, Col}) ->
    {Type, 0, {Row, Col}, get_rot(Type, 0)}.

% rotates a tetromino
% If Direction = 1, rotates clockwise, if -1, rotates counter clockwise
% 
% Each tetromino piece will have a hard coded list of Cells: each direciton 
% has a different set of cells. Since the cells are relative to the center, we 
% can simply replace the cells in a tetromino tuple to rotate.
% 
% 
rotate_tetromino(Direction, {Type, Rotation, {CenterRow, CenterCol}, _Cells}) ->
    
    % we only support rotating a T right now--we will add a case statement here 
    % for each type of piece and potential call piece-specific rotate functions
    

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

% draw tetromino
draw_tetromino({Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win) ->
    set_color(Type),
    draw_tetris_square({CenterRow, CenterCol}, Win),
    lists:foreach(fun ({R, C}) -> draw_tetris_square({R + CenterRow, C + CenterCol}, Win) end, Cells),
    cecho:refresh().

% delete tetromino
% We have to remove a tetromino before redrawing it every time we make a move.
% Right now, the background is not set, so this makes it look like a 
% gray trail is always following the piece
delete_tetromino({_Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win) ->
    cecho:attron(?ceCOLOR_PAIR(234)), % gray
    draw_square({CenterRow, CenterCol}, Win),
    lists:foreach(fun ({R, C}) -> draw_square({R + CenterRow, C + CenterCol}, Win) end, Cells).

% set color for each piece before printing, based on piece type
set_color(Type) ->
    case Type of 
        t -> Color = 92; % PURPLE
        square -> Color = 3; % YELLOW
        left -> Color = 203; % ORANGE
        right -> Color = 4; % BLUE
        zigz -> Color = 1;
        zags -> Color = 2;
        line -> Color = 39
    end, cecho:attron(?ceCOLOR_PAIR(Color)).

draw_tetris_square({Row, Col}, {WinY, WinX}) ->
    cecho:mvaddstr(Row + WinY, Col + WinX, "[]"), ok.   

draw_square({Row, Col}, {WinY, WinX}) ->
    cecho:mvaddstr(Row + WinY, Col + WinX, "  ").

move_left({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol - 2}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_down({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow + 1, CenterCol}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_right({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol + 2}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

% move tetromino to specified coordinate (will be used for implementing space)
% move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) when Row < 0 ->
%     {Type, Rotation, {0, Col}, Cells};
% move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) when Col < 0 ->
%     {Type, Rotation, {Row, 0}, Cells};
move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) ->
    {Type, Rotation, {Row, Col}, Cells}.

process_key(Key, Tetromino, Win) -> 
    case Key of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        ?ceKEY_UP ->
            delete_tetromino(Tetromino, Win),
            NewT = rotate_tetromino(1, Tetromino),
            draw_tetromino(NewT, Win),
            {Win, NewT};
        ?ceKEY_LEFT ->
            delete_tetromino(Tetromino, Win),
            NewT = move_left(Tetromino),
            % NewT = check_bounds(NewT, Tetromino),
            draw_tetromino(NewT, Win),
            {Win, NewT};
        ?ceKEY_RIGHT ->
            delete_tetromino(Tetromino, Win),
            NewT = move_right(Tetromino),
            draw_tetromino(NewT, Win),
            {Win, NewT};
        ?ceKEY_DOWN ->
            delete_tetromino(Tetromino, Win),
            NewT = move_down(Tetromino),
            draw_tetromino(NewT, Win),
            {Win, NewT};
        $z -> 
            delete_tetromino(Tetromino, Win),
            NewT = rotate_tetromino(-1, Tetromino),
            draw_tetromino(NewT, Win),
            {Win, NewT};
        ?KEY_RESIZE ->
            delete_tetromino(Tetromino, Win),  % Delete old tetromino
            NewWin = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
            draw_tetromino(Tetromino, NewWin),
            {NewWin, Tetromino};
        _ -> 
            {Win, Tetromino}
    end.

wait_for_input(Tetromino, Win) ->
    receive
        {_Pid, key, $q} -> 
            tetris_io:stop(),
            io:format("Thanks for playing!~n");
        {_Pid, key, Key} -> 
            {NewWin, NewTetromino} = process_key(Key, Tetromino, Win),
            wait_for_input(NewTetromino, NewWin);
        Other ->
            io:format("Client Unexpected msg: ~p~n", [Other])
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
    