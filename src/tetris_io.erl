-module(tetris_io).

% Tetris IO Module - implements low level interfacing with cecho

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

% Control functions
-export([init/0, stop/0, spawn_keyboard_proc/0, set_auto_refresh/2,
         set_resize_recipient/2]).
% Drawing functions
-export([draw_board/2, draw_tetromino/2, delete_tetromino/3,
         draw_title_screen/2, draw_centered_message/3, draw_ghost/3,
         paint_screen/1, paint_box/3, draw_preview/3,
         draw_end_game_screen/1]).
% Animations/Interactive
-export([text_box/3, animate_clear_row/4]).
% Utilities
-export([calc_win_coords/2, set_color/1]).

% Initialize cecho, return {KeyboardPid, RefreshPid, MaxRow, MaxCol}
init() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:keypad(?ceSTDSCR, true),
    pair_creation(),
    {MaxRow, MaxCol} = cecho:getmaxyx(),

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

% Main loop for keyboard process
keyboard_loop(Client_Pid) ->
    Key = cecho:getch(),
    % TODO: FILTER OUT UNIMPORTANT KEY INPUT
    Client_Pid ! {self(), key, Key},
    keyboard_loop(Client_Pid).

% Main loop for refresh process
refresh_loop(Recipient, {MaxY, MaxX}, AutoRefresh) ->
    receive
        {autorefresh, NewAutoRefresh} -> refresh_loop(Recipient, {MaxY, MaxX},
                                                      NewAutoRefresh);
        {setrecipient, NewRecipient} -> refresh_loop(NewRecipient, {MaxY, MaxX},
                                                  AutoRefresh)
    after 1000 ->
        case AutoRefresh of
            true -> cecho:refresh();
            _ -> ok
        end,
        {NewMaxY, NewMaxX} = cecho:getmaxyx(),
        case {NewMaxY, NewMaxX} of
            {MaxY, MaxX} -> ok;
            _ -> Recipient! {self(), resize}
        end,
        refresh_loop(Recipient, {NewMaxY, NewMaxX}, AutoRefresh)
    end.

% Spawn the keyboard process that waits for keypresses and sends a message to
% the current process
spawn_keyboard_proc() ->
    Client = self(),
    spawn_link(fun () -> keyboard_loop(Client) end).

% Spawn the refresh process that monitors for screen size changes and sends out
% notifications
spawn_refresh_proc() ->
    Client = self(),
    spawn_link(fun () -> refresh_loop(Client, cecho:getmaxyx(), true) end).

% Configure auto refresh, i.e. whether the refresh proc calls cecho:refresh
% itself or leaves it up to another proc
set_auto_refresh(RefreshPid, Enable) ->
    RefreshPid ! {autorefresh, Enable}.

% Set which process receives resize notifications
set_resize_recipient(RefreshPid, Recipient) ->
    RefreshPid ! {setrecipient, Recipient}.

draw_tetris_square({Row, Col}, {WinY, WinX, _, _}) ->
    cecho:mvaddstr(Row + WinY, Col + WinX, "[]"), ok.   

draw_square({Row, Col}, {WinY, WinX, _, _}, Color) ->
    set_color(Color),
    cecho:mvaddstr(Row + WinY, Col + WinX, "  ").

% Compute a window of a given width and height given the current terminal size
% Window: {Y, X, Width, Height}
calc_win_coords(Width, Height) ->
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    BeginX = (MaxCol - Width * 2) div 2,
    BeginY = (MaxRow - Height) div 2,
    {BeginY, BeginX, Width, Height}.

% Redraw a full board given the board and a window
draw_board(Board, Win) -> 
    Rows = lists:enumerate(Board),
    lists:map(fun ({Row, Cells}) ->
                   array:map(fun (Col, {_, Color}) ->
                                  draw_square({Row - 1, Col * 2}, Win, Color) 
                             end, Cells)
              end, Rows).

% Draw all the tetromino square in the current color
draw_tetrmonino_squares(T, Win) ->
    Coords = tetromino:get_all_coords(T),
    lists:foreach(fun ({R, C}) -> 
                        draw_tetris_square({R, C * 2}, Win) end, Coords),
    cecho:refresh().

% Draw a tetromino given the piece and window
draw_tetromino(T={Type, _Rotation, {_CenterRow, _CenterCol}, _Cells}, Win) ->
    set_color(Type),
    draw_tetrmonino_squares(T, Win).

% Draw a tetromino as a ghost
draw_ghost(T, Win, _Board) ->
    set_color(ghost),
    draw_tetrmonino_squares(T, Win).

% Overwrite all cells taken up by a tetromino with the board contents at those
% locations, erasing it
delete_tetromino({_Type, _Rotation, {CenterRow, CenterCol}, Cells}, Win,
                 Board) ->
    draw_square({CenterRow, CenterCol * 2}, Win,
                board:get_color(Board, CenterRow, CenterCol)),
    lists:foreach(
        fun ({R, C}) -> 
            draw_square({R + CenterRow, (C + CenterCol) * 2}, Win,
            board:get_color(Board, R + CenterRow, C + CenterCol)) 
        end,
        Cells).

% Draw a solid colored box at the given absolute coordinates on the screen of
% the current color
paint_box(Coord, Width, Height) -> 
    Spaces = lists:duplicate(Width, ?KEY_SPACE),
    List = lists:duplicate(Height, 10),
    lists:foldl(
        fun (_, {Row, Col}) ->
            cecho:mvaddstr(Row, Col, Spaces), {Row + 1, Col}
        end,
        Coord, List).

% Draw the preview of upcoming pieces given the preview itself, a window, and
% the background color
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
                    Coords = tetromino:get_abs_coords(
                        tetromino:change_center(P, {R, NewC})),
                    lists:foreach(
                        fun (Coord) ->
                            draw_tetris_square_abs(Coord)
                        end,
                        Coords),
                    R + 3
                  end, Row + 2, Preview).

% Draw a tetris square at a given pair of absolute coordinates
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
        logo -> 10;
        gold_text -> ?GOLD_TEXT;
        gold -> ?GOLD;
        silver_text -> ?SILVER_TEXT;
        silver -> ?SILVER;
        bronze_text -> ?BRONZE_TEXT;
        bronze -> ?BRONZE
    end, cecho:attron(?ceCOLOR_PAIR(Color)).



%%% pair_creation()
%%% Generates color pairs that will be used throughout the program
pair_creation() ->
    % lists:foldl(fun (T, Acc) -> cecho:init_pair()
    ok = cecho:start_color(),
    ok = cecho:init_pair(1, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    ok = cecho:init_pair(2, ?ceCOLOR_BLACK, ?ceCOLOR_GREEN),
    ok = cecho:init_pair(3, ?ceCOLOR_BLACK, ?ceCOLOR_YELLOW),
    ok = cecho:init_pair(4, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    ok = cecho:init_pair(5, ?ceCOLOR_BLACK, ?ceCOLOR_MAGENTA),
    ok = cecho:init_pair(6, ?ceCOLOR_BLACK, ?ceCOLOR_CYAN),
    % Title text
    ok = cecho:init_pair(7, ?ceCOLOR_BLACK, ?TITLE_BGD_COLOR),
    ok = cecho:init_pair(8, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    ok = cecho:init_pair(9, ?ceCOLOR_BLACK, 9),
    % Title logo
    ok = cecho:init_pair(10, 27, ?TITLE_BGD_COLOR),
    ok = cecho:init_pair(11, ?ceCOLOR_BLACK, ?SCREEN_BGD_COLOR),
    ok = cecho:init_pair(60, ?ceCOLOR_BLACK, 60), % GRAY
    ok = cecho:init_pair(203, ?ceCOLOR_BLACK, 203), % ORANGE
    ok = cecho:init_pair(200, ?ceCOLOR_BLACK, 200), % ORANGE
    ok = cecho:init_pair(92, ?ceCOLOR_BLACK, 92), % PURPLE
    ok = cecho:init_pair(?BACKGROUND_COLOR, ?ceCOLOR_BLACK,
                         ?BACKGROUND_COLOR), % BACKGROUND,
    ok = cecho:init_pair(39, ?ceCOLOR_BLACK, 39), % BACKGROUND
    ok = cecho:init_pair(?BORDER_COLOR, ?ceCOLOR_BLACK, ?BORDER_COLOR),
    ok = cecho:init_pair(?GHOST_COLOR, ?ceCOLOR_WHITE, ?BACKGROUND_COLOR),
    ok = cecho:init_pair(?GOLD_TEXT, ?GOLD, ?BACKGROUND_COLOR),
    ok = cecho:init_pair(?SILVER_TEXT, ?SILVER, ?BACKGROUND_COLOR),
    ok = cecho:init_pair(?BRONZE_TEXT, ?BRONZE, ?BACKGROUND_COLOR),
    ok = cecho:init_pair(?GOLD, ?ceCOLOR_BLACK, ?GOLD),
    ok = cecho:init_pair(?SILVER, ?ceCOLOR_BLACK, ?SILVER),
    ok = cecho:init_pair(?BRONZE, ?ceCOLOR_BLACK, ?BRONZE).

% Fill the entire screen with a given color
paint_screen(ColorType) ->
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    XCoords = lists:seq(0, MaxCol, 2),
    YCoords = lists:seq(0, MaxRow),
    XYCoords = [{X,Y} || X <- XCoords, Y <- YCoords],
    lists:foreach(
        fun ({Col, Row}) ->
            draw_square({Row, Col}, {0, 0, MaxCol, MaxRow}, ColorType)
        end,
        XYCoords),
    cecho:refresh().

% Draw a message centered in a window at row Row with a list of strings, one
% for each line
draw_centered_message(_, _, []) ->
    ok;
draw_centered_message(Row, {WinY, WinX, WinWidth, WinHeight}, [Line | LineT]) ->
    cecho:mvaddstr(Row + WinY,
                   ((WinWidth * 2) - string:length(Line)) div 2 + WinX,
                   Line),
    draw_centered_message(Row + 1, {WinY, WinX, WinWidth, WinHeight}, LineT).

% Run the row clear animation given a list of rows, a delay between frames,
% a window, and the length horizontally
animate_clear_row(RowNums, Sleep, Win, Length) ->
    animate_clear_row_r(RowNums, Sleep, Win, Length, 0).

% Same arguments as above plus an index argument
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

% Draw a title screen (with logo), given a window and the text to display
draw_title_screen(TitleWin={WinY, WinX, Width, Height}, CenteredMessage) ->
    paint_screen(title),
    set_color(logo),

    cecho:mvaddstr(WinY, WinX, "+"),
    cecho:mvaddstr(WinY + Height, WinX, "+"),
    cecho:mvaddstr(WinY, WinX + (Width * 2), "+"),
    cecho:mvaddstr(WinY + Height, WinX + (Width * 2), "+"),

    draw_centered_message(1, TitleWin, ?TITLE_LOGO),

    set_color(title),
    draw_centered_message(11, TitleWin, CenteredMessage),
    cecho:refresh().

% Loop for waiting for text box input given existing input and the box width
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
        {_Pid, key, ?ceKEY_BACKSPACE}
            when length(PrevInput) == (Width * 2) - 1 ->
                {Y, X} = cecho:getyx(),
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
        {_Pid, key, Key} when length(PrevInput) < (Width * 2) - 2 ->
            cecho:refresh(),
            get_text_box_input([Key | [PrevChar | PrevInput]], Width);
        {_Pid, key, Key} when length(PrevInput) == (Width * 2) - 2 ->
            {Y, X} = cecho:getyx(),
            cecho:move(Y, X - 1),
            cecho:refresh(),
            get_text_box_input([Key | [PrevChar | PrevInput]], Width);
        {_Pid, key, Key} -> {Y, X} = cecho:getyx(),
                            cecho:move(Y, X - 1),
                            cecho:refresh(),
                            get_text_box_input([Key | PrevInput], Width)
    end.

% Generate a text box of a given width, returning a list of strings
generate_box(Width) ->
    Line = lists:concat(["+", lists:duplicate(Width * 2, $-), "+"]),
    CenterRow = lists:concat(["|", lists:duplicate(Width * 2, ?KEY_SPACE),
                              "|"]),
    [Line, CenterRow, Line].

% Draw a text box where the typing location is at Row and of
% Width (2-col units), then return what the user typed
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

% Draw the ranking screen
draw_end_game_screen(Rankings) ->
    cecho:refresh(),
    paint_screen(border),
    NewWin = calc_win_coords(10, ?BOARD_HEIGHT),
    {WinR, WinC, _, _} = NewWin,    
    set_color(bg),
    RankHeight = length(Rankings) + 1,
    paint_box({WinR, WinC}, 19, RankHeight),
    set_color(ghost),
    cecho:mvaddstr(WinR, WinC, "Rankings:"),
    print_rankings(Rankings, {WinR + 1, WinC}, 0),

    set_color(bg),
    {_, PodiumC, _, _} =
        calc_win_coords(16, WinR + RankHeight + 1 + 10),
    PodiumBoxCoords = {PodiumR=WinR + RankHeight + 1, PodiumC},
    paint_box(PodiumBoxCoords, 29, 10),

    draw_podiums(Rankings, {PodiumR, PodiumC}, 0),
    set_color(border),
    draw_centered_message(15, NewWin, ["Press [Space] to continue"]),
    cecho:refresh().

% Draw podiums given players, the row and column, and the ranking index
draw_podiums(_, _, 3) -> ok;
draw_podiums([], _, _) -> ok;
draw_podiums([{Name, _Pid} | T], {P_R, P_C}, Idx) -> 
    {{PodiumR, PodiumC}, Color, Msg} = case Idx of 
        0 -> {{P_R + 5, P_C + 10}, gold, [" \\o/ ", "  |  ", " / \\ "]}; 
        1 -> {{P_R + 6, P_C + 1}, silver, ["  o7 ", " /|  ", " / \\ "]}; 
        2 -> {{P_R + 7, P_C + 19}, bronze, ["  o  ", " /|\\ ", " / \\ "]}
    end, 
    set_color(Color),
    paint_box({PodiumR, PodiumC}, 9, 5 - Idx),
    draw_centered_message(4 - Idx, {PodiumR, PodiumC, 4, 5}, [Name]),
    Guy = {PodiumR - 3, PodiumC, 5, 3},
    set_color(ghost),
    draw_centered_message(0, Guy, Msg),
    draw_podiums(T, {P_R, P_C}, Idx + 1).

% Print the ranking display
print_rankings([], _, _) -> ok;
print_rankings([{Name, _Pid} | T], Win={R, C}, Idx) ->
    case Idx of
        0 -> set_color(gold_text);
        1 -> set_color(silver_text);
        2 -> set_color(bronze_text);
        _ -> set_color(ghost)
    end, 
    Str = " " ++ integer_to_list(Idx + 1) ++ ". " ++ Name,
    cecho:mvaddstr(R, C, Str),
    print_rankings(T, {R + 1, C}, Idx + 1).