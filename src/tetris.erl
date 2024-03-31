-module(tetris).

-include_lib("../cecho/include/cecho.hrl").

-export([hello_world/0]).

hello_world() -> 
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
    ok = cecho:init_pair(60, ?ceCOLOR_BLACK, 60),
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    cecho:move(10,10),
    {EndVertRow, EndVertCol} = add_vert_line(45, 49, 10),
    cecho:attron(?ceCOLOR_PAIR(9)),
    add_check_horiz_line(EndVertRow, EndVertCol + 1, 10),
    cecho:attron(?ceCOLOR_PAIR(2)),
    draw_t(50, 50),

    %% print out a ton of colors in a line
    % Color = add_horiz_line_c(30, 30, 80, 10),
    % cecho:addstr(io_lib:format("~p", [Color])),

    cecho:move(0, 0),
    cecho:addch($ ),
    cecho:move(0, 1),
    cecho:addch($ ),
    cecho:move(MaxRow-1, 0),
    cecho:addch($@),
    cecho:move(0, MaxCol-1),
    cecho:addch($@),
    cecho:move(MaxRow-1, MaxCol-1),
    cecho:addch($@),
    cecho:refresh(),
    wait_for_input().
    

draw_t(Row, Col) ->
    draw_tetris_square(Row, Col),
    draw_tetris_square(Row, Col + 2),
    draw_tetris_square(Row, Col + 4),
    draw_tetris_square(Row + 1, Col + 2).


draw_tetris_square(Row, Col) ->
    cecho:mvaddstr(Row, Col, "[]").

draw_square(Row, Col) ->
    cecho:mvaddstr(Row, Col, "  ").

wait_for_input() ->
    C = cecho:getch(),
    case C of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        $q -> 
            application:stop(cecho);
        _ -> 
            wait_for_input()
    end.

add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
add_horiz_line_c(Row, Col, Length, ColorNum) ->
    cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
    cecho:attron(?ceCOLOR_PAIR(ColorNum)),
    cecho:mvaddch(Row, Col, $ ),
    add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).

add_horiz_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
add_horiz_line(Row, Col, Length) when Length rem 2 == 0 ->
    cecho:attron(?ceCOLOR_PAIR(60)),
    cecho:mvaddch(Row, Col, $ ),
    add_horiz_line(Row, Col + 1, Length - 1);
add_horiz_line(Row, Col, Length) when Length rem 2 == 1 ->
    cecho:attron(?ceCOLOR_PAIR(8)),
    cecho:mvaddch(Row, Col, $ ),
    add_horiz_line(Row, Col + 1, Length - 1).

add_check_horiz_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
add_check_horiz_line(Row, Col, Length) when Length rem 2 == 0 ->
    cecho:attron(?ceCOLOR_PAIR(60)),
    draw_square(Row, Col),
    add_check_horiz_line(Row, Col + 2, Length - 1);
add_check_horiz_line(Row, Col, Length) when Length rem 2 == 1 ->
    cecho:attron(?ceCOLOR_PAIR(8)),
    draw_square(Row, Col),
    add_check_horiz_line(Row, Col + 2, Length - 1).

add_vert_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
add_vert_line(Row, Col, Length) ->
    cecho:mvaddch(Row, Col, $|),
    add_vert_line(Row + 1, Col, Length - 1).
    


% hello_world() ->
% %% Start application
%     %% Start application
%     application:start(cecho),
    
%     %% Set attributes
%     cecho:cbreak(),
%     cecho:noecho(),
%     cecho:curs_set(?ceCURS_INVISIBLE),
%     %% Write initial string...
%     cecho:mvaddstr(0, 0, "Hello World!"),
%     cecho:refresh(),
%     %% Start the process that will "move" the string
%     Mover = spawn(fun() -> mvhello() end),
%     ctrl(Mover).

% ctrl(Mover) ->
%     %% get key-input
%     C = cecho:getch(),
%     case C of
% 	$q -> 
% 	    %% If we get a 'q' then exit the mover and stop cecho
% 	    exit(Mover, normal),
% 	    application:stop(cecho),
% 	    erlang:halt();
% 	_ ->
% 	    %% ignore anything else
% 	    ctrl(Mover)
%     end.

% %% start the mover
% mvhello() -> mvhello(0, 0, 1, 1).
% %% take previous pos and direction and print out new string
% mvhello(PrevY, PrevX, DirY, DirX) ->
%     %% "erase" previous position
%     cecho:mvaddstr(PrevY, PrevX, "            "),
%     %% calculate new position and direction
%     {NewY, NewX, NewDirY, NewDirX} =
% 	calc_new_pos(PrevY, PrevX, DirY, DirX),
%     %% "move" the text to new position
%     cecho:mvaddstr(NewY, NewX, "Hello World!"),
%     %% update the screen to show the change
%     cecho:refresh(),
%     %% do it again!
%     timer:sleep(50),
%     mvhello(NewY, NewX, NewDirY, NewDirX).

% calc_new_pos(Py, Px, Dy, Dx) ->
%     %% get max coords of the screen
%     {My, Mx} = cecho:getmaxyx(),
%     %% calc new vertical position and new direction
%     {NewPy, NewDy} =
% 	if (Py+(Dy) >= My) orelse (Py+(Dy) < 0) ->
% 		{Py+(Dy*-1), Dy*-1};
% 	   true ->
% 		{Py+(Dy), Dy}
% 	end,
%     %% calc new horizontal position and new direction
%     %% take string length into account
%     {NewPx, NewDx} =
% 	if (Px+(Dx)+12 >= Mx) orelse (Px+(Dx) < 0) ->
% 		{Px+(Dx*-1), Dx*-1};
% 	   true ->
% 		{Px+(Dx), Dx}
% 	end,
%     {NewPy, NewPx, NewDy, NewDx}.
