-module(tetris_io).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([init/0, stop/0, spawn_keyboard_proc/0, set_auto_refresh/2, set_resize_recipient/2, calc_game_win_coords/2, draw_board/2, draw_tetromino/2, delete_tetromino/3, draw_title_screen/2, draw_centered_message/3, draw_ghost/3, text_box/3]).
-export([animate_clear_row/4, paint_screen/1, draw_preview/3, set_color/1]).

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

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % keep this
    %% print out a ton of colors in a line
    % peach = 200
    % Color = add_horiz_line_c(30, 0, 91, 0),
    % cecho:addstr(io_lib:format("~p", [Color])),
    % 
    % BACKGROUND COLOR 235!!
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    KeyPid = spawn_keyboard_proc(),
    RefreshPid = spawn_refresh_proc(),
    {KeyPid, RefreshPid, MaxRow, MaxCol}.

stop() ->
    application:stop(cecho).

keyboard_loop(Client_Pid) ->
    Key = cecho:getch(),
    % TODO: FILTER OUT UNIMPORTANT KEY INPUT
    Client_Pid ! {self(), key, Key},
    case Key of 
        % $q ->
        %     ok;
        _ -> 
            keyboard_loop(Client_Pid)
    end.

refresh_loop(MainPid, {MaxY, MaxX}, AutoRefresh) ->
    receive
        {autorefresh, NewAutoRefresh} -> refresh_loop(MainPid, {MaxY, MaxX},
                                                      NewAutoRefresh);
        {setrecipient, Recipient} -> refresh_loop(Recipient, {MaxY, MaxX}, AutoRefresh)
    after 1000 ->
        case AutoRefresh of
            true -> cecho:refresh();
            _ -> ok
        end,
        {NewMaxY, NewMaxX} = cecho:getmaxyx(),
        case {NewMaxY, NewMaxX} of
            {MaxY, MaxX} -> ok;
            _ -> MainPid ! {self(), resize}
        end,
        refresh_loop(MainPid, {NewMaxY, NewMaxX}, AutoRefresh)
    end.

spawn_keyboard_proc() ->
    Client = self(),
    spawn_link(fun () -> keyboard_loop(Client) end).

spawn_refresh_proc() ->
    Client = self(),
    spawn_link(fun () -> refresh_loop(Client, cecho:getmaxyx(), true) end).

% Configure auto refresh, i.e. whether the refresh proc calls cecho:refresh itself or leaves it up to another proc
set_auto_refresh(RefreshPid, Enable) ->
    RefreshPid ! {autorefresh, Enable}.

set_resize_recipient(RefreshPid, Recipient) ->
    RefreshPid ! {setrecipient, Recipient}.

draw_tetris_square({Row, Col}, {WinY, WinX, _, _}) ->
    cecho:mvaddstr(Row + WinY, Col + WinX, "[]"), ok.   

draw_square({Row, Col}, {WinY, WinX, _, _}, Color) ->
    set_color(Color),
    cecho:mvaddstr(Row + WinY, Col + WinX, "  ").

%%% calc_game_win_coords(Width, Height)
%%% 
%%% 
calc_game_win_coords(Width, Height) ->
    % io:format("Width: ~p~n", [Width]),
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    BeginX = (MaxCol - Width * 2) div 2,
    BeginY = (MaxRow - Height) div 2,
    {BeginY, BeginX, Width, Height}.

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

draw_ghost({Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win, _Board) ->
    T = {Type, _Rotation, {CenterRow, CenterCol}, Cells},
    Coords = tetromino:get_all_coords(T),
    set_color(ghost),
    lists:foreach(fun ({R, C}) -> 
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

paint_box(Coord, Width, Height) -> 
    % io:format("coord: ~p~n", [Coord]),
    Spaces = lists:duplicate(Width, ?KEY_SPACE),
    List = lists:duplicate(Height, 10),
    lists:foldl(fun (_, {Row, Col}) -> cecho:mvaddstr(Row, Col, Spaces), {Row + 1, Col} end, Coord, List).

draw_preview(Preview, Win, Color) ->
    set_color(Color),
    {RowW, ColW, _, _} = Win,
    {Row, Col} = {RowW + 10, ColW - 11},
    paint_box({Row, Col}, 10, 10),
    MidC = Col + 4,
    lists:foldl(fun (P, R) -> 
                    Type = tetromino:type(P),
                    set_color(Type),
                    NewC = case Type of 
                        T when T == line; T == square -> MidC + 1;
                        _ -> MidC
                    end,
                    Coords = tetromino:get_abs_coords(tetromino:change_center(P, {R, NewC})),
                    lists:foreach(fun (Coord) -> draw_tetris_square_abs(Coord) end, Coords),
                    R + 3
                  end, Row + 2, Preview).

draw_tetris_square_abs({Row, Col}) ->
    cecho:mvaddstr(Row, Col, "[]"), ok.  

% set color for each piece before printing, based on piece type
set_color(Type) ->
    Color = case Type of 
        ghost -> ?GHOST_COLOR;
        t -> ?T_COLOR; % PURPLE
        square -> ?SQUARE_COLOR; % YELLOW
        left -> ?LEFT_COLOR; % ORANGE
        right -> ?RIGHT_COLOR; % BLUE
        zigz -> ?ZIGZ_COLOR;
        zags -> ?ZAGS_COLOR;
        line -> ?LINE_COLOR;
        bg   -> ?BACKGROUND_COLOR;  % Should this be ?BACKGROUND_COLOR
        border -> ?BORDER_COLOR;
        bigboy -> 200;
        scrbg -> 11;
        title -> 7;
        clear -> 7;
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
    ok = cecho:init_pair(7, ?ceCOLOR_BLACK, ?TITLE_BGD_COLOR),
    ok = cecho:init_pair(8, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    ok = cecho:init_pair(9, ?ceCOLOR_BLACK, 9),
    ok = cecho:init_pair(10, 27, ?TITLE_BGD_COLOR),
    ok = cecho:init_pair(11, ?ceCOLOR_BLACK, ?SCREEN_BGD_COLOR),
    ok = cecho:init_pair(60, ?ceCOLOR_BLACK, 60), % GRAY
    ok = cecho:init_pair(203, ?ceCOLOR_BLACK, 203), % ORANGE
    ok = cecho:init_pair(200, ?ceCOLOR_BLACK, 200), % ORANGE
    ok = cecho:init_pair(92, ?ceCOLOR_BLACK, 92), % PURPLE
    ok = cecho:init_pair(?BACKGROUND_COLOR, ?ceCOLOR_BLACK, ?BACKGROUND_COLOR), % BACKGROUND,
    ok = cecho:init_pair(39, ?ceCOLOR_BLACK, 39), % BACKGROUND
    ok = cecho:init_pair(?BORDER_COLOR, ?ceCOLOR_BLACK, ?BORDER_COLOR),
    ok = cecho:init_pair(?GHOST_COLOR, ?ceCOLOR_WHITE, ?BACKGROUND_COLOR).


paint_screen(ColorType) ->
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    XCoords = lists:seq(0, MaxCol, 2),
    YCoords = lists:seq(0, MaxRow),
    XYCoords = [{X,Y} || X <- XCoords, Y <- YCoords],
    lists:foreach(fun ({Col, Row}) -> draw_square({Row, Col}, {0, 0, MaxCol, MaxRow}, ColorType) end, XYCoords),
    cecho:refresh().

draw_centered_message(_, _, []) ->
    ok;
draw_centered_message(Row, {WinY, WinX, WinWidth, WinHeight}, [Line | LineT]) ->
    cecho:mvaddstr(Row + WinY, ((WinWidth * 2) - string:length(Line)) div 2 + WinX, Line),
    draw_centered_message(Row + 1, {WinY, WinX, WinWidth, WinHeight}, LineT).

animate_clear_row(RowNums, Sleep, Win, Length) -> animate_clear_row_r(RowNums, Sleep, Win, Length, 0).

animate_clear_row_r([], _, _, _, _) ->
    ok;
animate_clear_row_r(_, _Sleep, _Win,  Length, Length) ->
    cecho:refresh();
animate_clear_row_r(RowNums, Sleep, Win, Length, 0) ->
    lists:foreach(fun (R) -> 
        draw_square({R, 0}, Win, clear) end, RowNums),
    cecho:refresh(),
    timer:sleep(Sleep),
    animate_clear_row_r(RowNums, Sleep, Win, Length, 1);
animate_clear_row_r(RowNums, Sleep, Win, Length, Curr) ->
    lists:foreach(fun (R) -> 
        draw_square({R, (Curr - 1) * 2}, Win, bg) end, RowNums),
    lists:foreach(fun (R) -> 
        draw_square({R, Curr * 2}, Win, clear) end, RowNums),
    cecho:refresh(),
    timer:sleep(Sleep),
    animate_clear_row_r(RowNums, Sleep, Win, Length, Curr + 1).

draw_title_screen({WinY, WinX, Width, Height}, CenteredMessage) ->
    TitleWin = {WinY, WinX, Width, Height},
    paint_screen(title),
    % draw_logo(0, (?TITLESCR_WIDTH - ?LOGO_WIDTH), TitleWin),
    set_color(logo),

    cecho:mvaddstr(WinY, WinX, "+"),
    cecho:mvaddstr(WinY + Height, WinX, "+"),
    cecho:mvaddstr(WinY, WinX + (Width * 2), "+"),
    cecho:mvaddstr(WinY + Height, WinX + (Width * 2), "+"),

    draw_centered_message(1, TitleWin, ?TITLE_LOGO),

    set_color(title),
    draw_centered_message(11, TitleWin, CenteredMessage),
    cecho:refresh().

get_text_box_input([], Width) ->
    receive
        {_Pid, key, $\n} -> "";
        {_Pid, key, ?ceKEY_BACKSPACE} ->  {Y, X} = cecho:getyx(),
                                          cecho:move(Y, X + 1),
                                          cecho:refresh(),
                                          get_text_box_input([], Width);
        {_Pid, key, Key} -> cecho:refresh(),
                            get_text_box_input([Key], Width)
    end;
get_text_box_input([PrevChar | PrevInput], Width) ->
    receive
        {_Pid, key, $\n} -> lists:reverse([PrevChar | PrevInput]);
        {_Pid, key, ?ceKEY_BACKSPACE} when length(PrevInput) == (Width * 2) - 1 -> {Y, X} = cecho:getyx(),
                                         cecho:move(Y, X + 1),
                                         cecho:addch(?KEY_SPACE),
                                         cecho:move(Y, X + 1),
                                         cecho:refresh(),
                                         get_text_box_input(PrevInput, Width);
        {_Pid, key, ?ceKEY_BACKSPACE} -> {Y, X} = cecho:getyx(),
                                         cecho:addch(?KEY_SPACE),
                                         cecho:move(Y, X),
                                         cecho:refresh(),
                                         get_text_box_input(PrevInput, Width);
        {_Pid, key, Key} when length(PrevInput) < (Width * 2) - 2 -> cecho:refresh(),
                                                                     get_text_box_input([Key | [PrevChar | PrevInput]], Width);
        {_Pid, key, Key} when length(PrevInput) == (Width * 2) - 2 -> {Y, X} = cecho:getyx(),
                            cecho:move(Y, X - 1),
                            cecho:refresh(),
                            get_text_box_input([Key | [PrevChar | PrevInput]], Width);
        {_Pid, key, Key} -> {Y, X} = cecho:getyx(),
                            cecho:move(Y, X - 1),
                            cecho:refresh(),
                            get_text_box_input([Key | PrevInput], Width)
    end.

generate_box(Width) ->
    Line = lists:concat(["+", lists:duplicate(Width * 2, $-), "+"]),
    CenterRow = lists:concat(["|", lists:duplicate(Width * 2, ?KEY_SPACE), "|"]),
    [Line, CenterRow, Line].

% Draw a text box where the typing location is at Row and of Width (2-col units), then return what the user typed
text_box({WinY, WinX, WinWidth, WinHeight}, Width, Row) ->
    Win = {WinY, WinX, WinWidth, WinHeight},
    draw_centered_message(Row - 1, Win, generate_box(Width)),
    ok = cecho:curs_set(?ceCURS_NORMAL),
    ok = cecho:echo(),
    cecho:move(WinY + Row, WinX + ((WinWidth - Width))),
    cecho:refresh(),
    Input = get_text_box_input([], Width),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:noecho(),
    Input.