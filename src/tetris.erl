-module(tetris).

-include_lib("../cecho/include/cecho.hrl").

-export([hello_world/0]).

hello_world() ->
%% Start application
    %% Start application
    application:start(cecho),
    %% Set attributes
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    %% Write initial string...
    cecho:mvaddstr(0, 0, "Hello World!"),
    cecho:refresh(),
    %% Start the process that will "move" the string
    Mover = spawn(fun() -> mvhello() end),
    ctrl(Mover).

ctrl(Mover) ->
    %% get key-input
    C = cecho:getch(),
    case C of
	$q -> 
	    %% If we get a 'q' then exit the mover and stop cecho
	    exit(Mover, normal),
	    application:stop(cecho),
	    erlang:halt();
	_ ->
	    %% ignore anything else
	    ctrl(Mover)
    end.

%% start the mover
mvhello() -> mvhello(0, 0, 1, 1).
%% take previous pos and direction and print out new string
mvhello(PrevY, PrevX, DirY, DirX) ->
    %% "erase" previous position
    cecho:mvaddstr(PrevY, PrevX, "            "),
    %% calculate new position and direction
    {NewY, NewX, NewDirY, NewDirX} =
	calc_new_pos(PrevY, PrevX, DirY, DirX),
    %% "move" the text to new position
    cecho:mvaddstr(NewY, NewX, "Hello World!"),
    %% update the screen to show the change
    cecho:refresh(),
    %% do it again!
    timer:sleep(50),
    mvhello(NewY, NewX, NewDirY, NewDirX).

calc_new_pos(Py, Px, Dy, Dx) ->
    %% get max coords of the screen
    {My, Mx} = cecho:getmaxyx(),
    %% calc new vertical position and new direction
    {NewPy, NewDy} =
	if (Py+(Dy) >= My) orelse (Py+(Dy) < 0) ->
		{Py+(Dy*-1), Dy*-1};
	   true ->
		{Py+(Dy), Dy}
	end,
    %% calc new horizontal position and new direction
    %% take string length into account
    {NewPx, NewDx} =
	if (Px+(Dx)+12 >= Mx) orelse (Px+(Dx) < 0) ->
		{Px+(Dx*-1), Dx*-1};
	   true ->
		{Px+(Dx), Dx}
	end,
    {NewPy, NewPx, NewDy, NewDx}.
