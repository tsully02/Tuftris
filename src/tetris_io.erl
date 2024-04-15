-module(tetris_io).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([init/0, stop/0, spawn_keyboard_proc/0, calc_game_win_coords/2, draw_board/2, draw_tetromino/2, delete_tetromino/3]).

init() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:keypad(?ceSTDSCR, true),
    pair_creation(),
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    cecho:move(10,10),
    % {EndVertRow, EndVertCol} = add_vert_line(45, 49, 10),
    cecho:attron(?ceCOLOR_PAIR(9)),
    % add_check_horiz_line(EndVertRow, EndVertCol + 1, 10),
    cecho:attron(?ceCOLOR_PAIR(2)),
        % timer:sleep(1000),
    % delete_tetromino(T),
    % T2 = rotate_tetromino_clock(T),
    % draw_tetromino(T2),
    % cecho:refresh(),
    % io:format("~p~n", [T]),
    % io:format("~p~n", [T2]),
    % % draw_t(50, 50),
    % timer:sleep(1000),

    % delete_tetromino(T2),
    % T3 = rotate_tetromino_clock(T2),
    % draw_tetromino(T3),
    % cecho:refresh(),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % keep this
    %% print out a ton of colors in a line
    % peach = 200
    % Color = add_horiz_line_c(30, 0, 91, 0),
    % cecho:addstr(io_lib:format("~p", [Color])),
    % 
    % BACKGROUND COLOR 235!!
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    cecho:move(0, 0),
    cecho:addch($@),
    cecho:move(MaxRow-1, 0),
    cecho:addch($@),
    cecho:move(0, MaxCol-1),
    cecho:addch($@),
    cecho:move(MaxRow-1, MaxCol-1),
    cecho:addch($@),

    KeyPid = spawn_keyboard_proc(),
    {KeyPid, MaxRow, MaxCol}.

stop() ->
    application:stop(cecho).

keyboard_loop(Client_Pid) ->
    Key = cecho:getch(),
    % TODO: FILTER OUT UNIMPORTANT KEY INPUT
    Client_Pid ! {self(), key, Key},
    case Key of 
        $q ->
            ok;
        _ -> 
            keyboard_loop(Client_Pid)
    end. 

spawn_keyboard_proc() ->
    Client = self(),
    spawn_link(fun () -> keyboard_loop(Client) end).

% add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
% add_horiz_line_c(Row, Col, Length, ColorNum) ->
%     cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
%     cecho:attron(?ceCOLOR_PAIR(ColorNum)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).


draw_tetris_square({Row, Col}, {WinY, WinX}) ->
    cecho:mvaddstr(Row + WinY, Col + WinX, "[]"), ok.   

draw_square({Row, Col}, {WinY, WinX}, Color) ->
    cecho:attron(?ceCOLOR_PAIR(Color)),
    cecho:mvaddstr(Row + WinY, Col + WinX, "  ").

%%% calc_game_win_coords(Width, Height)
%%% 
%%% 
calc_game_win_coords(Width, Height) ->
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    BeginX = (MaxCol - Width * 2) div 2,
    BeginY = (MaxRow - Height) div 2,
    {BeginY, BeginX}.

draw_board(Board, Win) -> 
    Rows = lists:enumerate(Board),
    lists:map(fun ({Row, Cells}) ->
                   array:map(fun (Col, {_, Color}) ->
                                  draw_square({Row - 1, Col * 2}, Win, Color) 
                             end, Cells)
              end, Rows).

% draw tetromino
draw_tetromino({Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win) ->
    set_color(Type),
    draw_tetris_square({CenterRow, CenterCol * 2}, Win),
    lists:foreach(fun ({R, C}) -> draw_tetris_square({R + CenterRow, C * 2 + CenterCol * 2}, Win) end, Cells),
    cecho:refresh().

% delete tetromino
% We have to remove a tetromino before redrawing it every time we make a move.
% Right now, the background is not set, so this makes it look like a 
% gray trail is always following the piece
delete_tetromino({_Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win, Board) ->
    % cecho:attron(?ceCOLOR_PAIR(?BACKGROUND_COLOR)), % gray
    draw_square({CenterRow, CenterCol * 2}, Win, board:get_color(Board, CenterRow, CenterCol)),
    lists:foreach(fun ({R, C}) -> 
        draw_square({R + CenterRow, (C + CenterCol) * 2}, Win, board:get_color(Board, R + CenterRow, C + CenterCol)) end, Cells).


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

%%% pair_creation()
%%% Generates color pairs that will be used throughout the proga
pair_creation() ->
    ok = cecho:start_color(),
    ok = cecho:init_pair(1, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    ok = cecho:init_pair(2, ?ceCOLOR_BLACK, ?ceCOLOR_GREEN),
    ok = cecho:init_pair(3, ?ceCOLOR_BLACK, ?ceCOLOR_YELLOW),
    ok = cecho:init_pair(4, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    ok = cecho:init_pair(5, ?ceCOLOR_BLACK, ?ceCOLOR_MAGENTA),
    ok = cecho:init_pair(6, ?ceCOLOR_BLACK, ?ceCOLOR_CYAN),
    ok = cecho:init_pair(7, ?ceCOLOR_BLACK, ?ceCOLOR_WHITE),
    ok = cecho:init_pair(8, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    ok = cecho:init_pair(9, ?ceCOLOR_BLACK, 9),
    ok = cecho:init_pair(60, ?ceCOLOR_BLACK, 60), % GRAY
    ok = cecho:init_pair(203, ?ceCOLOR_BLACK, 203), % ORANGE
    ok = cecho:init_pair(92, ?ceCOLOR_BLACK, 92), % PURPLE
    ok = cecho:init_pair(?BACKGROUND_COLOR, ?ceCOLOR_BLACK, ?BACKGROUND_COLOR), % BACKGROUND
    ok = cecho:init_pair(39, ?ceCOLOR_BLACK, 39). % BACKGROUND