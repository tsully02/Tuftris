-module(tetromino).

-include_lib("tetris.hrl").

-export([get_all_coords/1, rotate/2, generate/4, check_bounds/1, check_collision/2, move_left/1, move_right/1, move_down/1, move_to_lowest_position/2]).


% Convert a tetromino to a list of board coordinates it takes up
get_all_coords({_Type, _Rotation, {CRow, CCol}, Cells}) ->
    WinCells = lists:map(fun ({Row, Col}) -> {CRow + Row, CCol + Col} end, Cells),
    [{CRow, CCol} | WinCells].


%%% generate(Pid, Time, {Row, Col})
%%% generates a random new tetromino
%%% TODO: Make less random :)
%%% 
%%% the double code this is temporary i swear...
generate(Pid, Time, {Row, Col}, line) ->
    TimerPid = spawn(fun () -> tetris:timer(Pid, Time) end),
    % io:format("Timer: ~p~n", [TimerPid]),
    T = {line, 0, {Row, Col}, get_rot(line, 0)},
    {T, TimerPid};
generate(Pid, Time, {Row, Col}, Pid) ->
    Tetrominos = [t, line, zigz, zags, square, left, right],
    % random:seed(erlang:now()),
    Type = lists:nth(rand:uniform(7), Tetrominos),
    TimerPid = spawn(fun () -> tetris:timer(Pid, Time) end),
    % io:format("Timer: ~p~n", [TimerPid]),
    T = {Type, 0, {Row, Col}, get_rot(Type, 0)},
    {T, TimerPid};
generate(Pid, Time, {Row, Col}, GameRoomPid) ->
    Tetrominos = [t, line, zigz, zags, square, left, right],
    % random:seed(erlang:now()),
    Type = lists:nth(rand:uniform(7), Tetrominos),
    TimerPid = spawn(fun () -> tetris:timer(Pid, Time) end),
    % io:format("Timer: ~p~n", [TimerPid]),
    T = {Type, 0, {Row, Col}, get_rot(Type, 0)},
    GameRoomPid ! {newpiece, T, self()},
    {T, TimerPid}.


% rotates a tetromino
% If Direction = 1, rotates clockwise, if -1, rotates counter clockwise
% 
% Each tetromino piece will have a hard coded list of Cells: each direciton 
% has a different set of cells. Since the cells are relative to the center, we 
% can simply replace the cells in a tetromino tuple to rotate.
% 
% 
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

%%% change_line_center({CenterRow, CenterCol}, ChangeIndex)
%%% 
change_line_center({CenterRow, CenterCol}, ChangeIndex) ->
    List = ?Rotation_Line_Centers,
    {ChangeRow, ChangeCol} = lists:nth(ChangeIndex + 1, List),
    {CenterRow + ChangeRow, CenterCol + ChangeCol}.

% Take a tetromino and adjust its bounds to keep it in bounds, then return the new tetromino
check_bounds({Type, Rotation, {CRow, CCol}, Cells}) ->
    Tet = {Type, Rotation, {CRow, CCol}, Cells},
    PredL = fun ({_Row, Col}) -> Col < 0 end,
    PredR = fun ({_Row, Col}) -> Col > ?BOARD_WIDTH - 1 end,
    Fun = case lists:map(fun (Pred) -> lists:any(Pred, get_all_coords(Tet)) end, [PredL, PredR]) of
        [true, _] -> fun (T) -> check_bounds(move_right(T)) end;
        [_, true] -> fun (T) -> check_bounds(move_left(T)) end;
        _ -> fun (T) -> T end
    end,
    Fun(Tet).

% Return whether a tetromino is overlapping any filled cells on the board or against the bottom
check_collision({Type, Rotation, {CRow, CCol}, Cells}, Board) ->
    T = {Type, Rotation, {CRow, CCol}, Cells},
    TCoords = get_all_coords(T),
    BottomPred = fun ({Row, _Col}) -> Row > ?BOARD_HEIGHT - 1 end,
    OnBottom = lists:any(BottomPred, TCoords),
    OnBottom orelse lists:any(fun ({Row, Col}) -> board:is_filled(Board, Row, Col) end, TCoords).


move_left({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol - 1}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_down({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow + 1, CenterCol}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

% move_up({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
%     move_tetromino({CenterRow - 1, CenterCol}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_right({Type, Rotation, {CenterRow, CenterCol}, Cells}) ->
    move_tetromino({CenterRow, CenterCol + 1}, {Type, Rotation, {CenterRow, CenterCol}, Cells}).

move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) ->
    {Type, Rotation, {Row, Col}, Cells}.

move_to_lowest_position(Tetromino, Board) ->
    DownTetromino = tetromino:move_down(Tetromino),
    case tetromino:check_collision(DownTetromino, Board) of
        true -> Tetromino;
        false -> move_to_lowest_position(DownTetromino, Board)
    end.