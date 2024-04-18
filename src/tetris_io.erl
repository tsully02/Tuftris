-module(tetris_io).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([init/0, stop/0, spawn_keyboard_proc/0, calc_game_win_coords/2, draw_board/2, draw_tetromino/2, delete_tetromino/3, title_screen/1, draw_ghost/3]).
-export([animate_clear_row/3]).

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
    set_color(Color),
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

draw_ghost({Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win, Board) ->
    T = {Type, _Rotation, {CenterRow, CenterCol}, Cells},
    Coords = tetromino:get_all_coords(T),
    lists:foreach(fun ({R, C}) -> 
                        set_color(board:get_color(Board, R, C)),
                        draw_tetris_square({R, C * 2}, Win) end, Coords),
    cecho:refresh().

% get_color(Type) ->
%     case Type of
%         t -> ?T_COLOR; % PURPLE
%         square -> ?SQUARE_COLOR; % YELLOW
%         left -> ?LEFT_COLOR; % ORANGE
%         right -> ?RIGHT_COLOR; % BLUE
%         zigz -> ?ZIGZ_COLOR;
%         zags -> ?ZAGS_COLOR;
%         line -> ?LINE_COLOR;
%         bg -> ?BACKGROUND_COLOR
%     end.
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
    Color = case Type of 
        t -> ?T_COLOR; % PURPLE
        square -> ?SQUARE_COLOR; % YELLOW
        left -> ?LEFT_COLOR; % ORANGE
        right -> ?RIGHT_COLOR; % BLUE
        zigz -> ?ZIGZ_COLOR;
        zags -> ?ZAGS_COLOR;
        line -> ?LINE_COLOR;
        bg   -> ?BACKGROUND_COLOR;  % Should this be ?BACKGROUND_COLOR
        scrbg -> 11;
        title -> 7;
        logo -> 10
    end, cecho:attron(?ceCOLOR_PAIR(Color)).



%%% pair_creation()
%%% Generates color pairs that will be used throughout the proga
pair_creation() ->
    % TColors = [?T_COLOR, ?SQUARE_COLOR, ?LEFT_COLOR, ?RIGHT_COLOR, ?ZIGZ_COLOR, 
    %     ?ZAGS_COLOR, ?LINE_COLOR],
    
    % lists:foldl(fun (T, Acc) -> cecho:init_pair()
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
    ok = cecho:init_pair(10, ?ceCOLOR_MAGENTA, ?ceCOLOR_WHITE),
    ok = cecho:init_pair(11, ?ceCOLOR_BLACK, ?SCREEN_BACKGROUND_COLOR),
    ok = cecho:init_pair(60, ?ceCOLOR_BLACK, 60), % GRAY
    ok = cecho:init_pair(203, ?ceCOLOR_BLACK, 203), % ORANGE
    ok = cecho:init_pair(92, ?ceCOLOR_BLACK, 92), % PURPLE
    ok = cecho:init_pair(?BACKGROUND_COLOR, ?ceCOLOR_BLACK, ?BACKGROUND_COLOR), % BACKGROUND
    ok = cecho:init_pair(39, ?ceCOLOR_BLACK, 39). % BACKGROUND


clear_screen(ColorType) ->
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    XCoords = lists:seq(0, MaxCol, 2),
    YCoords = lists:seq(0, MaxRow),
    XYCoords = [{X,Y} || X <- XCoords, Y <- YCoords],
    lists:foreach(fun ({Col, Row}) -> draw_square({Row, Col}, {0, 0}, ColorType) end, XYCoords).


draw_centered_message(_, _, _, []) ->
    ok;
draw_centered_message(Row, {WinY, WinX}, WinWidth, [Line | LineT]) ->
    cecho:mvaddstr(Row + WinY, ((WinWidth * 2) - string:length(Line)) div 2 + WinX, Line),
    draw_centered_message(Row + 1, {WinY, WinX}, WinWidth, LineT).


title_screen_keyboard_loop() ->
    receive
        {_Pid, key, $1} -> start;
        {_Pid, key, $2} -> server:create_room('tetris_server@vm-hw06.eecs.tufts.edu', "testroom", "amelia", 3),
            start;
        {_Pid, key, $q} -> quit;
        {_Pid, key, _} -> title_screen_keyboard_loop()
    end.

animate_clear_row([], _, _) ->
    ok;
animate_clear_row(RowNums, Win, 10) ->
    lists:foreach(fun (R) -> 
        draw_square({R, 18}, Win, bg) end, RowNums),
    cecho:refresh();
animate_clear_row(RowNums, Win, 0) ->
    lists:foreach(fun (R) -> 
        draw_square({R, 0}, Win, title) end, RowNums),
    cecho:refresh(),
    timer:sleep(40),
    animate_clear_row(RowNums, Win, 1);
animate_clear_row(RowNums, Win, Curr) ->
    lists:foreach(fun (R) -> 
        draw_square({R, (Curr - 1) * 2}, Win, bg) end, RowNums),
    lists:foreach(fun (R) -> 
        draw_square({R, Curr * 2}, Win, title) end, RowNums),
    cecho:refresh(),
    timer:sleep(40),
    animate_clear_row(RowNums, Win, Curr + 1).

title_screen({WinY, WinX}) ->
    TitleWin = {WinY, WinX},
    clear_screen(title),
    % draw_logo(0, (?TITLESCR_WIDTH - ?LOGO_WIDTH), TitleWin),
    set_color(logo),

    cecho:mvaddstr(WinY, WinX, "+"),
    cecho:mvaddstr(WinY + ?TITLESCR_HEIGHT, WinX, "+"),
    cecho:mvaddstr(WinY, WinX + (?TITLESCR_WIDTH * 2), "+"),
    cecho:mvaddstr(WinY + ?TITLESCR_HEIGHT, WinX + (?TITLESCR_WIDTH * 2), "+"),

    draw_centered_message(1, TitleWin, ?TITLESCR_WIDTH, ?TITLE_LOGO),

    set_color(title),
    draw_centered_message(11, TitleWin, ?TITLESCR_WIDTH, ["Press:", "1 Singleplayer", "2 Multiplayer", "q to quit"]),
    cecho:refresh(),
    Status = title_screen_keyboard_loop(),
    clear_screen(scrbg),
    cecho:refresh(),
    Status.

