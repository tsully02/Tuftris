-module(tetris_io).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([init/0, stop/0, spawn_keyboard_proc/0, calc_game_win_coords/2]).

init() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:keypad(?ceSTDSCR, true),
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
    ok = cecho:init_pair(39, ?ceCOLOR_BLACK, 39), % BACKGROUND
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


%%% calc_game_win_coords(Width, Height)
%%% 
%%% 
calc_game_win_coords(Width, Height) ->
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    BeginX = (MaxCol - Width * 2) div 2,
    BeginY = (MaxRow - Height) div 2,
    {BeginY, BeginX}.
