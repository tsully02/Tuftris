-module(board).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([create_board/3, get_color/3, is_filled/3, place_piece/2]).

create_board(Width, Height, _Color) ->
    List = list_of_arrays(Height, [], Width),
    lists:map(fun (Arr) -> array:map(fun (_, _) -> {false, ?ceCOLOR_CYAN} end, Arr) end, List).
    
    % Arr = array:new(Height),
    % array:map(fun (_, _) ->
    %     array:map(fun (_, _) ->
    %         {false, ?ceCOLOR_YELLOW}
    %     end, array:new(Width)) end, Arr).

list_of_arrays(0, List, _ArrLen) -> List;
list_of_arrays(LLen, List, ArrLen) ->
    list_of_arrays(LLen - 1, lists:append(List, [array:new(ArrLen)]), ArrLen).

get_cell(Board, Row, Col) ->
    % io:format("Board: ~p~n", [Board]),
    Arr = lists:nth(Row + 1, Board),
    array:get(Col, Arr).

set_cell(Board, Val, Row, Col) ->
    set_cell_r(Board, Val, Row, Col, 0).

set_cell_r([], _, _, _, _) -> [];
set_cell_r([BH | BT], Val, Row, Col, Row) -> 
    [array:set(Col, Val, BH) | set_cell_r(BT, Val, Row, Col, Row + 1)];
set_cell_r([BH | BT], Val, Row, Col, Curr) -> 
    [BH | set_cell_r(BT, Val, Row, Col, Curr + 1)].


is_filled(Board, Row, Col) ->
    {Filled, _Type} = get_cell(Board, Row, Col),
    Filled.

get_color(Board, Row, Col) ->
    {_Filled, Type} = get_cell(Board, Row, Col),
    Type.

%%% place_piece
%%% Takes falling piece and places it in the board state
place_piece(Board, Tetromino) ->
    Piece = tetris:get_tetromino_cell_coords(Tetromino),
    {Type, _Rot, _Center, _Cells} = Tetromino,
    lists:foldl(fun ({Row, Col}, BoardAcc) -> set_cell(BoardAcc, {true, Type}, Row, Col) end, Board, Piece).
