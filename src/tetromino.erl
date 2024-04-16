-module(tetromino).

-export([get_all_coords/1]).

% Convert a tetromino to a list of board coordinates it takes up
get_all_coords({_Type, _Rotation, {CRow, CCol}, Cells}) ->
    WinCells = lists:map(fun ({Row, Col}) -> {CRow + Row, CCol + Col} end, Cells),
    [{CRow, CCol} | WinCells].