-module(board).

% Board module - functionality for manipulating game boards

% Boards are 0-indexed in both dimensions for the purposes of all interfaces

-include_lib("tetris.hrl").

-export([create/3, get_color/3, is_filled/3, place_piece/2, remove_row/2,
         remove_rand_row/2, create_random/2]).

% Create a new board of the given width and height filled with the given color
create(Width, Height, Color) ->
    List = list_of_arrays(Height, [], Width),
    lists:map(
        fun (Arr) ->
            array:map(fun (_, _) ->
                          {false, Color}
                      end,
                      Arr)
        end,
        List).

% Create a new board of the given width and height filled with random colors
create_random(Width, Height) ->
    List = list_of_arrays(Height, [], Width),
    Colors = [t, square, left, right, zigz, zags, line],
    lists:map(
        fun (Arr) ->
            array:map(fun (_, _) ->
                          {false, lists:nth(rand:uniform(7), Colors)}
                      end,
                      Arr)
        end,
        List).

% Create a list of arrays given the length of the list, the accumulated list,
% and the length of the inner arrays
list_of_arrays(0, List, _ArrLen) -> List;
list_of_arrays(LLen, List, ArrLen) ->
    list_of_arrays(LLen - 1, [array:new(ArrLen) | List], ArrLen).

% Get the raw value of the board at a given row and column
get_cell(Board, Row, Col) ->
    % Internally, the board is a list of arrays and arrays are 0-indexed while
    % lists are 1-indexed
    Arr = lists:nth(Row + 1, Board),
    array:get(Col, Arr).

% Replace the value in a board at a given set of coordinates
set_cell(Board, Val, Row, Col) ->
    set_cell_r(Board, Val, Row, Col, 0).

% Helper for function above with current index as last argument
set_cell_r([], _, _, _, _) -> [];
set_cell_r([BH | BT], Val, Row, Col, Row) -> 
    [array:set(Col, Val, BH) | set_cell_r(BT, Val, Row, Col, Row + 1)];
set_cell_r([BH | BT], Val, Row, Col, Curr) -> 
    [BH | set_cell_r(BT, Val, Row, Col, Curr + 1)].

% Get whether a given position on the board if filled
is_filled(Board, Row, Col) ->
    {Filled, _Type} = get_cell(Board, Row, Col),
    Filled.

% Get the color of a given position on the board
get_color(Board, Row, Col) ->
    {_Filled, Type} = get_cell(Board, Row, Col),
    Type.

%%% place_piece
%%% Takes falling piece and places it in the board state
place_piece(Board, Tetromino) ->
    Piece = tetromino:get_all_coords(Tetromino),
    {Type, _Rot, _Center, _Cells} = Tetromino,
    lists:foldl(
        fun ({Row, Col}, BoardAcc) ->
            set_cell(BoardAcc, {true, Type}, Row, Col)
        end,
        Board, Piece).

% Remove a given row from the baord and insert a new row returned by passing
% a given function the width
remove_row_f(Board, Row, NewRowFunc) ->
    {Above, Below} = lists:split(Row + 1, Board),
    NewAbove = lists:droplast(Above),
    [BottomRow | _] = Board,
    ArrLen = array:size(BottomRow),
    [NewRowFunc(ArrLen) | lists:append(NewAbove, Below)].

% Remove a given row from the board and insert a new empty row at the top
remove_row(Board, Row) ->
    remove_row_f(Board, Row, fun generate_empty_row/1).

% Remove a given row from the board and insert a new random row at the top
remove_rand_row(Board, Row) ->
    remove_row_f(Board, Row, fun generate_rand_row/1).

% Generate a single random row
generate_rand_row(Width) ->
    Arr = array:new(Width),
    Colors = [t, square, left, right, zigz, zags, line],
    array:map(
        fun (_, _) ->
            {false, lists:nth(rand:uniform(7), Colors)}
        end,
        Arr).

% Generate a single empty row
generate_empty_row(Width) ->
    Arr = array:new(Width),
    array:map(fun (_, _) -> {false, bg} end, Arr).
