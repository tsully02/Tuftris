-module(tetris).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([hello_world/0]).

hello_world() -> 
    {KeyPid, MaxRow, MaxCol} = tetris_io:init(),

    T = generate_tetromino(line, {MaxRow div 2, MaxCol div 2}),
    draw_tetromino(T),
    wait_for_input(T).

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
%%% Right now the program places a T in the middle of the screen, and supports 
%%% the following keyboard inputs (see wait_for_input/1)
%%%     q: quit
%%%     Up arrow: rotate clockwise
%%%     Z: rotate counter clockwise
%%%     Right arrow: move right
%%%     Left arrow: move left
%%%     Down arrow: move down

generate_tetromino(Type, {Row, Col}) ->
    {Type, 0, {Row, Col}, get_rot(Type, 0)}.

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
draw_tetromino({Type, _Rotation, {CenterRow, CenterCol}, Cells}) ->
    set_color(Type),
    draw_tetris_square({CenterRow, CenterCol}),
    lists:foreach(fun ({R, C}) -> draw_tetris_square({R + CenterRow, C + CenterCol}) end, Cells),
    cecho:refresh().

% delete tetromino
% We have to remove a tetromino before redrawing it every time we make a move.
% Right now, the background is not set, so this makes it look like a 
% gray trail is always following the piece
delete_tetromino({_Type, _Rotation, {CenterRow, CenterCol}, Cells}) ->
    cecho:attron(?ceCOLOR_PAIR(60)), % gray
    draw_square({CenterRow, CenterCol}),
    lists:foreach(fun ({R, C}) -> draw_square({R + CenterRow, C + CenterCol}) end, Cells).

% set color for each piece before printing, based on piece type
set_color(Type) ->
    case Type of 
        t -> Color = 92; % PURPLE
        square -> Color = 3; % YELLOW
        left -> Color = 203; % ORANGE
        right -> Color = 4; % BLUE
        zigz -> Color = 1;
        zags -> Color = 2;
        line -> Color = 3
    end, cecho:attron(?ceCOLOR_PAIR(Color)).

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
    {Type, NewR, {CenterRow, CenterCol}, get_rot(Type, NewR)}.

draw_tetris_square({Row, Col}) ->
    cecho:mvaddstr(Row, Col, "[]"), ok.

draw_square({Row, Col}) ->
    cecho:mvaddstr(Row, Col, "  ").

move_left({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol - 2}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_down({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow + 1, CenterCol}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_right({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol + 2}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

% move tetromino to specified coordinate (will be used for implementing space)
move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) ->
    {Type, Rotation, {Row, Col}, Cells}.

process_key(Key, Tetromino) -> 
    case Key of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        ?ceKEY_UP ->
            delete_tetromino(Tetromino),
            NewT = rotate_tetromino(1, Tetromino),
            draw_tetromino(NewT),
            NewT;
        ?ceKEY_LEFT ->
            delete_tetromino(Tetromino),
            NewT = move_left(Tetromino),
            draw_tetromino(NewT),
            NewT;
        ?ceKEY_RIGHT ->
            delete_tetromino(Tetromino),
            NewT = move_right(Tetromino),
            draw_tetromino(NewT),
            NewT;
        ?ceKEY_DOWN ->
            delete_tetromino(Tetromino),
            NewT = move_down(Tetromino),
            draw_tetromino(NewT),
            NewT;
        $z -> 
            delete_tetromino(Tetromino),
            NewT = rotate_tetromino(-1, Tetromino),
            draw_tetromino(NewT),
            NewT;
        ?KEY_RESIZE ->
            delete_tetromino(Tetromino),
            NewT = rotate_tetromino(-1, Tetromino),
            draw_tetromino(NewT),
            NewT;
        _ -> 
            Tetromino
    end.

wait_for_input(Tetromino) ->
    receive
        {_Pid, key, $q} -> 
            tetris_io:stop(),
            io:format("Thanks for playing!~n");
        {_Pid, key, Key} -> 
            NewTetromino = process_key(Key, Tetromino),
            wait_for_input(NewTetromino);
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
    