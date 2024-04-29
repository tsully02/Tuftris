%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% board.erl
%%% 
%%% Board Module: functionality for manipulating game boards
%%% 
%%% A board is a list of arrays: each array represents one row in the board. 
%%% 
%%% Index Notation: 
%%%     Indices are notated as {Row, Col}, and are 0-indexed in both dimensions 
%%% for the purposes of all interfaces (with {0, 0} being the top left cell 
%%% of the board).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(board).

-include_lib("tetris.hrl").

-export([create/3, get_color/3, is_filled/3, place_piece/2, remove_row/2,
         remove_rand_row/2, create_random/2]).


%%% create/3 creates a new board of the given width and height filled with
%%%          the given color.
-spec create(integer(), integer(), atom()) -> list().

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


%%% create_random/2 creates a new board of the given width and height filled
%%%                 with random colors. It is used to generate the background
%%%                 that shows that players have blocked out and lost the game.
-spec create_random(integer(), integer()) -> list().

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


%%% list_of_arrays/3 creates a list of arrays of a provided length.
-spec list_of_arrays(integer(), list(), integer()) -> list().

list_of_arrays(0, List, _ArrLen) -> List;
list_of_arrays(LLen, List, ArrLen) ->
    list_of_arrays(LLen - 1, [array:new(ArrLen) | List], ArrLen).


%%% get_cell/3 returns the raw value of the board at a given row and column.
%%%            Because lists are 1-indexed and arrays are 0-indexed, this
%%%            function performs the math to get the correct location.
-spec get_cell(list(), integer(), integer()) -> tuple().

get_cell(Board, Row, Col) ->
    Arr = lists:nth(Row + 1, Board),
    array:get(Col, Arr).


%%% set_cell/4 replaces the current value of a given board at the given set of
%%%            coordinates with the provided new value.
-spec set_cell(list(), tuple(), integer(), integer()) -> tuple().

set_cell(Board, Val, Row, Col) ->
    set_cell_r(Board, Val, Row, Col, 0).


%%% set_cell_r/5 is the helper function for set_cell. It takes the board, the
%%%              new value, the target row and column, and an accumulation
%%%              integer for the recursive search.
-spec set_cell_r(list(), tuple(), integer(), integer(), integer()) -> list().

set_cell_r([], _, _, _, _) -> [];
set_cell_r([BH | BT], Val, Row, Col, Row) -> 
    [array:set(Col, Val, BH) | set_cell_r(BT, Val, Row, Col, Row + 1)];
set_cell_r([BH | BT], Val, Row, Col, Curr) -> 
    [BH | set_cell_r(BT, Val, Row, Col, Curr + 1)].


%%% is_filled/3 returns if the given position on the board is occupied by a
%%%             piece
-spec is_filled(list(), integer(), integer()) -> atom().

is_filled(Board, Row, Col) ->
    {Filled, _Type} = get_cell(Board, Row, Col),
    Filled.


%%% get_color/3 returns the color of the cell at the given position on the board
-spec get_color(list(), integer(), integer()) -> atom().

get_color(Board, Row, Col) ->
    {_Filled, Type} = get_cell(Board, Row, Col),
    Type.


%%% place_piece/2 places a falling piece into the board state, marking those
%%%               cells are filled and returning the updated board
-spec place_piece(list(), tuple()) -> list()

place_piece(Board, Tetromino) ->
    Piece = tetromino:get_all_coords(Tetromino),
    {Type, _Rot, _Center, _Cells} = Tetromino,
    lists:foldl(
        fun ({Row, Col}, BoardAcc) ->
            set_cell(BoardAcc, {true, Type}, Row, Col)
        end,
        Board, Piece).


%%% remove_row_f/3 removes the given row from the board and inserts a new row
%%%                at the top. The new row will be empty or full depending on
%%%                the player and the board.
-spec remove_row_f(list(), integer(), function()) -> list().

remove_row_f(Board, Row, NewRowFunc) ->
    {Above, Below} = lists:split(Row + 1, Board),
    NewAbove = lists:droplast(Above),
    [BottomRow | _] = Board,
    ArrLen = array:size(BottomRow),
    [NewRowFunc(ArrLen) | lists:append(NewAbove, Below)].


%%% remove_row/2 removes the given row from the board and inserts a new empty
%%%              row at the top.
-spec remove_row_f(list(), integer()) -> list().

remove_row(Board, Row) ->
    remove_row_f(Board, Row, fun generate_empty_row/1).


%%% remove_rand_row/2 removes the given row from the board and inserts a new
%%%                   randomly filled row at the top.
-spec remove_rand_row(list(), integer()) -> list().
remove_rand_row(Board, Row) ->
    remove_row_f(Board, Row, fun generate_rand_row/1).


%%% generate_rand_row/1 generates a single random row.
-spec generate_rand_row(integer()) -> array().

generate_rand_row(Width) ->
    Arr = array:new(Width),
    Colors = [t, square, left, right, zigz, zags, line],
    array:map(
        fun (_, _) ->
            {false, lists:nth(rand:uniform(7), Colors)}
        end,
        Arr).


%%% generate_empty_row/1 generates a single empty row
-spec generate_empty_row(integer()) -> array().

generate_empty_row(Width) ->
    Arr = array:new(Width),
    array:map(fun (_, _) -> {false, bg} end, Arr).
