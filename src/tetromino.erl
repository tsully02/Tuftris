%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% tetromino.erl
%%%
%%% Tetromino module: Functionality for creating and manipulating tetrominos
%%% 
%%% Piece Tuple Notation:
%%% 
%%%     {Type, Rotation, Center, Cells}
%%% 
%%% Type: atom denoting the type of piece, which can be
%%%       t, square, left, right, zig, zag, line, or bigboy
%%%       (Note: bigboy was for debugging purposes)
%%% Rotation: integer between 0 and 3 denoting the direction (0 is original 
%%%           direction, 1 is 90° clockwise, 2 is 180° clockwise, etc.)
%%% Center: "Center" index of piece (not always the actual center)
%%% Cells: Location of cells, relative to the center (add the cell values to 
%%%        the center coords to get the cell coords)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(tetromino).
-include_lib("tetris.hrl").


% Creation
-export([generate/2, get_ghost/2]).
% Getting info
-export([get_all_coords/1, get_abs_coords/1, type/1, check_collision/2]).
% Moving
-export([rotate/2, check_bounds/1, move_left/1, move_right/1, move_down/1,
         move_to_lowest_position/2, change_center/2]).

-type tetromino() :: {line | zigz | zags | left | right | square | t,
                      0 | 1 | 2 | 3, {pos_integer(), pos_integer()},
                      [{integer(), integer()}]}.
-export_type([tetromino/0]).


% Convert a tetromino to a list of board coordinates it takes up
-spec get_all_coords(tuple()) -> list().
get_all_coords({_Type, _Rotation, {CRow, CCol}, Cells}) ->
    % io:format("CRow: ~p, CCol: ~p~n", [CRow, CCol]),
    WinCells = lists:map(
        fun ({Row, Col}) ->
            {CRow + Row, CCol + Col}
        end,
        Cells),
    [{CRow, CCol} | WinCells].

% Convert a tetromino to a list of character coordinates it takes up
% (relative to the board)
-spec get_abs_coords(tetromino()) -> list().
get_abs_coords({_Type, _Rotation, {CRow, CCol}, Cells}) ->
    WinCells = lists:map(
        fun ({Row, Col}) ->
            {CRow + Row, CCol + Col * 2}
        end,
        Cells),
    [{CRow, CCol} | WinCells].

% Move a tetromino so its center is at the given coordinates
-spec change_center(tetromino(), tetris_io:coord()) -> tetromino().
change_center({Type, Rotation, _, Cells}, {NewR, NewC}) ->
    {Type, Rotation, {NewR, NewC}, Cells}.

% Get the type of a piece
type(Piece) -> element(1, Piece).


%%% generate({Row, Col}, type)
%%% generates a random new tetromino
%%% Type can be something that does not name a type (like 'random) for a 
%%% random piece
-spec generate(tuple(), atom()) -> tetromino().
generate({Row, Col}, Type) when Type == line; Type == bigboy ->
    {Type, 0, {Row, Col}, get_rot(Type, 0)};
generate({Row, Col}, _Type) ->
    Tetrominos = [t, line, zigz, zags, square, left, right],
    % random:seed(erlang:now()),
    Type = lists:nth(rand:uniform(7), Tetrominos),
    {Type, 0, {Row, Col}, get_rot(Type, 0)}.


% rotates a tetromino
% If Direction = 1, rotates clockwise, if -1, rotates counter clockwise
% 
% Each tetromino piece will have a hard coded list of Cells: each direciton 
% has a different set of cells. Since the cells are relative to the center, we 
% can simply replace the cells in a tetromino tuple to rotate.
% 
-spec rotate(integer(), tetromino()) -> tetromino().
rotate(Direction, {Type, Rotation, {CenterRow, CenterCol}, _Cells}) ->
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

%%% get_rot(Type, Rotation)
%%% Defines rotation based on given peice type
-spec get_rot(atom(), integer()) -> [tetris_io:coord()].
get_rot(Type, Rotation) ->
    case Type of 
        t -> List = ?Rotation_T;
        left -> List = ?Rotation_Left;
        right -> List = ?Rotation_Right;
        square -> List = ?Rotation_Square;
        zigz -> List = ?Rotation_Zigz;
        zags -> List = ?Rotation_Zags;
        line -> List = ?Rotation_Line;
        bigboy -> List = ?bigboy
    end,
    lists:nth(Rotation + 1, List).

%%% change_line_center({CenterRow, CenterCol}, ChangeIndex)
%%% Adjust the center of a line based on the change index defined by the
%%% rotation
-spec change_line_center(tuple(), integer()) -> tuple().
change_line_center({CenterRow, CenterCol}, ChangeIndex) ->
    List = ?Rotation_Line_Centers,
    {ChangeRow, ChangeCol} = lists:nth(ChangeIndex + 1, List),
    {CenterRow + ChangeRow, CenterCol + ChangeCol}.

% Take a tetromino and adjust its coordinates to keep it in bounds,
% then return the new tetromino
-spec check_bounds(tetromino()) -> tetromino().
check_bounds({Type, Rotation, {CRow, CCol}, Cells}) ->
    Tet = {Type, Rotation, {CRow, CCol}, Cells},
    PredL = fun ({_Row, Col}) -> Col < 0 end,
    PredR = fun ({_Row, Col}) -> Col > ?BOARD_WIDTH - 1 end,
    Fun =
        case lists:map(
            fun (Pred) ->
                lists:any(Pred, get_all_coords(Tet))
            end, [PredL, PredR])
        of
            [true, _] -> fun (T) -> check_bounds(move_right(T)) end;
            [_, true] -> fun (T) -> check_bounds(move_left(T)) end;
            _ -> fun (T) -> T end
        end,
    Fun(Tet).

% Return whether a tetromino is overlapping any filled cells on the board or
% below the bottom
-spec check_collision(tetromino(), list()) -> boolean().
check_collision({Type, Rotation, {CRow, CCol}, Cells}, Board) ->
    T = {Type, Rotation, {CRow, CCol}, Cells},
    TCoords = get_all_coords(T),
    BottomPred = fun ({Row, _Col}) -> Row > ?BOARD_HEIGHT - 1 end,
    OnBottom = lists:any(BottomPred, TCoords),
    % Short circuit here to avoid passing invalid rows to board:is_filled
    OnBottom orelse lists:any(fun ({Row, Col}) -> 
                                  board:is_filled(Board, Row, Col)
                              end,
                              TCoords).


% Move a tetromino left one cell
-spec move_left(tetromino()) -> tetromino().
move_left({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    change_center({Type, Rotation, {CenterRow, CenterCol}, Cells},
                  {CenterRow, CenterCol - 1}).

% Move a tetromino down one cell
-spec move_down(tetromino()) -> tetromino().
move_down({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    change_center({Type, Rotation, {CenterRow, CenterCol}, Cells},
                  {CenterRow + 1, CenterCol}).

% Move a tetromino right one cell
-spec move_right(tetromino()) -> tetromino().
move_right({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    change_center({Type, Rotation, {CenterRow, CenterCol}, Cells},
                  {CenterRow, CenterCol + 1}).

% Move a tetromino to the lowest position without it overlapping any other piece
-spec move_to_lowest_position(tetromino(), list()) -> tetromino().
move_to_lowest_position(Tetromino, Board) ->
    DownTetromino = move_down(Tetromino),
    case check_collision(DownTetromino, Board) of
        true -> Tetromino;
        false -> move_to_lowest_position(DownTetromino, Board)
    end.

% Get the "ghost" of a tetromino, i.e. the tetromino that should be drawn to
% show the white "ghost" piece
-spec get_ghost(tetromino(), list()) -> tetromino().
get_ghost(Tetromino, Board) ->
    move_to_lowest_position(Tetromino, Board).
