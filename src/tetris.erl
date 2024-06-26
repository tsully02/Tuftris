%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% tetris.erl
%%%
%%% Tetris/Client Module:
%%%     - includes the logic for the startup screen, and creating/joining a 
%%%       room
%%%     - includes the game logic for the client side, such as processing 
%%%       keyboard input, checking for cleared rows, and keeping the state 
%%%       of the game
%%% 
%%% As the game runs, this module has to keep track of several things. Here is 
%%% a list of the most common ones (the names persist throughout all functions):
%%%     - Painter: the pid of the painter process
%%%     - TimerPid: the pid of the timer process
%%%     - RefreshPid: the pid of the window resize-checking process
%%%     - GameRoom: the pid of the game room, which is currently being played
%%%     - Tetromino: the player's tetromino that is currently falling/in play,
%%%                  which is created and updated by the Tetromino Module
%%%     - Board: the current state of the player's board, as represented by 
%%%              the Board Module
%%% 
%%% This file is organized by the flow of the program: the functions that 
%%% handle the title screen and creating/joining rooms are first, then the 
%%% game logic functions, then miscellaneous functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(tetris).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([initiate/1, timer/2, get_game_width/1, clear_board_rows/2]).

% Title screen status: {GameInfo={GameRoom, NumPlayers}, RoomName}
-type title_scr_status() :: {game, {pid(), pos_integer()}} | quit.
-type preview() :: [tetromino:tetromino()].
-export_type([preview/0]).

%%% initialite/1 acts as the main function
-spec initiate([integer()]) -> ok.
initiate(UserName) ->
    Painter = spawn_link(fun () -> painter:start(UserName) end),
    {_KeyPid, RefreshPid, _MaxRow, _MaxCol} = tetris_io:init(),
    % Num = add_horiz_line_c(30, 0, 131, 0),
    % io:format("~p~n", [Num]),
    % cecho:refresh(),
    % receive_space("ope"),
    {Status, RoomName} = title_screen(UserName, Painter),
    tetris(Status, Painter, RefreshPid, RoomName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Title Screen %%%%%%%%%%

%%% title_screen/2 runs the title screen and returns the game status and room
-spec title_screen([integer()], pid()) -> {title_scr_status(), [integer()]}.
title_screen(UserName, Painter) ->
    Win = tetris_io:calc_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
    tetris_io:draw_title_screen(Win, ?TITLE_MSG),
    {Status, RoomName} = title_screen_keyboard_loop(UserName, Win, Painter),
    tetris_io:paint_screen(scrbg),
    {Status, RoomName}.

%%% title_screen_keyboard_loop/3 loops for keyboard input on the title screen
-spec title_screen_keyboard_loop([integer()], tetris_io:window(), pid()) ->
      {title_scr_status(), [integer()]}.
title_screen_keyboard_loop(UserName, Win, Painter) ->
    receive
        {_Pid, key, $1} -> 
            GameRoom = server:create_room(?SERVER_NODE, UserName, UserName, 1,
                                          Painter),
            case GameRoom of 
                already_exists -> 
                    Msg = "You are already playing a solo game! :P",
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win, Painter);
                _ -> {{game, {GameRoom, 1}}, UserName}
            end;
        {_Pid, key, $2} -> 
            % io:format("messaging server...~n"),
            create_multi_room(UserName, Win, Painter);
        {_Pid, key, $3} -> 
            join_multi_room(UserName, Win, Painter);
        {_Pid, key, $q} -> 
            Painter ! {lebron, jamie},
            {quit, UserName};
        {_Pid, resize} ->
            NewWin = tetris_io:calc_win_coords(?TITLESCR_WIDTH,
                                               ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, ?TITLE_MSG),
            title_screen_keyboard_loop(UserName, NewWin, Painter);
        % {_Pid, key, $t} -> tboxtest;
        {_Pid, key, _} -> title_screen_keyboard_loop(UserName, Win, Painter)
    end.

%%% create_multi_room/3 handles the multiplayer room creation sequence and
%%%                     returns the status
-spec create_multi_room([integer()], tetris_io:window(), pid()) ->
      {title_scr_status(), [integer()]}.
create_multi_room(UserName, Win, Painter) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    NumPlayersMsg = ["Enter the number of players (2-9)"],
    tetris_io:draw_title_screen(Win, NumPlayersMsg),
    NumPlayers = input_num_players(NumPlayersMsg),
    case NumPlayers of
        0 -> title_screen_keyboard_loop(UserName, Win, Painter);
        _ ->  GameRoom = server:create_room(?SERVER_NODE, RoomName, UserName,
                                            NumPlayers, Painter),
            case GameRoom of 
                already_exists -> 
                    Msg = lists:append(["Room ", RoomName, " already exists!"]),
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win, Painter);
                _ -> {{game, {GameRoom, NumPlayers}}, RoomName}
            end
    end.

%%% join_multi_room/3 handles the multiplayer room join sequence and
%%%                   returns the status
-spec join_multi_room([integer()], tetris_io:window(), pid()) ->
      {title_scr_status(), [integer()]}.
join_multi_room(UserName, Win, Painter) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    GameInfo = server:join_room(?SERVER_NODE, RoomName, UserName, Painter),
    case GameInfo of 
        room_full -> 
            Msg = lists:append(["Room ", RoomName, " is full, sorry :("]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win, Painter);
        no_such_room -> 
            Msg = lists:append(["Room ", RoomName, " does not exist!"]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win, Painter);
        _ -> {{game, GameInfo}, RoomName}
    end.

%%% get_num_players/1 prompts for the number of players and returns it, or 0 if
%%% the user quits
-spec input_num_players([[integer()]]) -> non_neg_integer().
input_num_players(Msg) -> 
    receive
        {_Pid, key, Key} when Key =< $9, Key >= $2 -> 
            Key - $0;
        {_Pid, key, $q} -> 
            0;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_win_coords(?TITLESCR_WIDTH,
                                               ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            input_num_players(Msg);
        {_Pid, key, _} -> input_num_players(Msg)
    end.

%%% error_title/2 displays an error message and waits for the space bar
-spec error_title(tetris_io:window(), [integer()]) -> ok.
error_title(Win, Msg) ->
    FullMsg = [Msg, "Press [Space] to continue"],
    tetris_io:draw_title_screen(Win, FullMsg),
    receive_space(FullMsg),
    tetris_io:draw_title_screen(Win, ?TITLE_MSG).

%%% receive_space/1 waits for a space keypress and redraws a title screen given
%%%                 a message on terminal resizes
-spec receive_space([string()]) -> ok.
receive_space(Msg) -> 
    receive
        {_Pid, key, ?KEY_SPACE}-> ok;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_win_coords(?TITLESCR_WIDTH,
                                               ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            receive_space(Msg);
        _ -> receive_space(Msg)
    end.

%%%%%%%%%% END TITLE SCREEN %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% TETRIS GAME LOGIC %%%%%%%%%% 

%%% tetris/4 takes in a status from the title screen logic and processes it,
%%%          starting the game or exiting
-spec tetris(title_scr_status(), pid(), pid(), [integer()]) -> ok.
tetris(quit, _, _, _) ->
    tetris_io:stop();
tetris({game, {GameRoom, NumPlayers}}, Painter, RefreshPid, RoomName) ->
    TitleWin = tetris_io:calc_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
    Msg = ["Waiting for players...", "Room Name: " ++ RoomName, "(q - Quit)"],
    tetris_io:draw_title_screen(TitleWin, Msg),
    Status = wait_to_start(TitleWin, GameRoom, Msg),
    tetris_io:set_auto_refresh(RefreshPid, false),
    tetris_io:set_resize_recipient(RefreshPid, Painter),
    start_game(GameRoom, NumPlayers, Painter, Status, RefreshPid).


%%% wait_to_start/3 waits for a start message from the game room
-spec wait_to_start(tetris_io:window(), pid(), [[integer()]]) -> quit | ok.
wait_to_start(TitleWin, GameRoom, Msg) ->
    receive
        {_Pid, start} -> ok;
        {_Pid, key, $q} ->
            tetris_io:stop(),
            io:format("Thanks for playing!~n"),
            quit;
        {_RefreshPid, resize} ->
            NewWin = tetris_io:calc_win_coords(?TITLESCR_WIDTH,
                                               ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            wait_to_start(NewWin, GameRoom, Msg);
        _ -> wait_to_start(TitleWin, GameRoom, Msg)
    end.


%%% start_game/5 starts the game given the result of wait_to_start
-spec start_game(pid(), pos_integer(), pid(), quit | ok, pid()) -> ok.
start_game(_, _, _, quit, _) -> ok;
start_game(GameRoom, NumPlayers, Painter, ok, RefreshPid) ->
    Win = tetris_io:calc_win_coords(get_game_width(NumPlayers), ?BOARD_HEIGHT),
    Board = board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, bg),
    T = tetromino:generate({1, 5}, random),
    % GameRoom ! {newpiece, T, self()},
    tetris_io:paint_screen(border),
    Preview = [tetromino:generate({1, 5}, random),
               tetromino:generate({1, 5}, random),
               tetromino:generate({1, 5}, random)],
    Painter ! {self(), winboardprev, Win, Board, Preview},
    Painter ! {self(), draw},
    Painter ! draw,
    Painter ! {self(), draw_tetromino, T, T},
    Painter ! {self(), draw_preview, Preview},
    Painter ! {self(), draw_tetromino, T, T},
    TimerPid = new_timer(self(), Painter),
    Pids = {TimerPid, GameRoom, Painter, RefreshPid},
    wait_for_input(T, Preview, Board, Pids).

%%% wait_for_input/4 acts as the main loop for keyboard input
-spec wait_for_input(tetromino:tetromino(), preview(), board:board(),
                     {pid(), pid(), pid(), pid()}) -> ok.
wait_for_input(Tetromino, Preview, Board,
               Pids={TimerPid, GameRoom, Painter, RefreshPid}) ->
    receive
        {TimerPid, timer} ->
            TGL_Pids = {TimerPid, GameRoom, Painter},
            Ret = process_key(?ceKEY_DOWN, Tetromino, Preview, Board, TGL_Pids),
            case Ret of
                blocked -> 
                    GameRoom ! {playerlost, self()},
                    lost_input(GameRoom, Painter, RefreshPid, TimerPid);
                {NewT, NewP, NewB, NewTimerPid} ->
                    NewPids = {NewTimerPid, GameRoom, Painter, RefreshPid},
                    wait_for_input(NewT, NewP, NewB, NewPids)
            end;
        {GameRoom, game_over, Rankings} ->
            end_game_screen(Rankings, GameRoom, Painter, RefreshPid, TimerPid);
        {_Pid, key, $q} -> 
            quit_game(GameRoom, Painter, RefreshPid, TimerPid);
        {_Pid, key, $l} -> 
            NewTetromino = tetromino:generate({1, 5}, line),
            Painter ! {self(), draw_tetromino, Tetromino, NewTetromino},
            wait_for_input(NewTetromino, Preview, Board, Pids);
        {_Pid, key, $a} -> 
            NewTetromino = tetromino:generate({1, 5}, bigboy),
            Painter ! {self(), draw_tetromino, Tetromino, NewTetromino},
            wait_for_input(NewTetromino, Preview, Board, Pids);
        {_Pid, key, Key} ->
            TGL_Pids = {TimerPid, GameRoom, Painter},
            case process_key(Key, Tetromino, Preview, Board, TGL_Pids) of
                blocked -> 
                    GameRoom ! {playerlost, self()},
                    lost_input(GameRoom, Painter, RefreshPid, TimerPid);
                {NewTetromino, NewPreview, NewBoard, NewTimerPid} ->
                    NewPids = {NewTimerPid, GameRoom, Painter, RefreshPid},
                    wait_for_input(NewTetromino, NewPreview, NewBoard, NewPids)
            end;
        {clearrow, Rows} -> 
            NewBoard = clear_row(Rows, Board, Painter, Tetromino),
            wait_for_input(Tetromino, Preview, NewBoard, Pids);
        {placepiece, _PInfo, _T} -> 
            wait_for_input(Tetromino, Preview, Board, Pids);
        {_Pid, timer} ->
            wait_for_input(Tetromino, Preview, Board, Pids);
        {_Pid, killtimer} ->
            wait_for_input(Tetromino, Preview, Board, Pids);
        serverdead ->
            tetris_io:stop(),
            ok;
        Other ->
            io:format("Client Unexpected msg: ~p~n", [Other])
    end.

%%% process_key/5
%%% 
%%% 
%%% 
%%% parameters: 
%%%     - Key: the keyboard input (as a character)
%%%     - Tetromino: the player's current, moving piece
%%%     - Board: the current the board
%%%     - Pids: a tuple of pids, which are (in their respective order):
%%%         - the Timer
%%%         - the Game Room
%%%         - The Painter
%%% 
%%% returns: {Tetromino, Preview, Board, Timer}, updated as needed
%%%     
%%%
-spec process_key(integer(), tetromino:tetromino(), preview(),
                  board:board(), {pid(), pid(), pid()}) ->
      {tetromino:tetromino(), preview(), board:board(), pid()}
      | blocked.
process_key(Key, Tetromino, Preview, Board, Pids={Timer, GameRoom, Painter}) ->
    ResultTetromino = case Key of 
        % using q here instead of ?ceKEY_ESC because the latter seems to be slow
        ?ceKEY_UP ->
            tetromino:rotate(1, Tetromino);
        ?ceKEY_LEFT ->
            tetromino:move_left(Tetromino);
        ?ceKEY_RIGHT ->
            tetromino:move_right(Tetromino);
        ?ceKEY_DOWN ->
            tetromino:move_down(Tetromino);
        ?KEY_SPACE ->
            tetromino:move_to_lowest_position(Tetromino, Board);
        $z -> 
            tetromino:rotate(-1, Tetromino);
        _ ->
            Tetromino
    end,
    NewT2 = tetromino:check_bounds(ResultTetromino),
    case tetromino:check_collision(NewT2, Board) of
        true ->  % Reject move, generate new tetromino if down
            {_Type, _Rotation, Center, _Cells} = Tetromino,
            {Row, _Col} = Center,
            case Row of
                1 -> blocked;
                _ ->
                    case Key of 
                        ?ceKEY_DOWN -> 
                            NewBoard = board:place_piece(Board, Tetromino),
                            Painter ! {self(), new_board, NewBoard},
                            GameRoom ! {placepiece, Tetromino, self()},
                            Painter ! {self(), draw},
                            Timer ! {self(), killtimer},
                            NewTimer = new_timer(self(), Painter),
                            check_clear_row(Tetromino, NewBoard, GameRoom),
                            {NewTetromino, NewPreview} =
                                next_piece(Preview, GameRoom),
                            Painter ! {self(), draw_preview, NewPreview},
                            Painter ! {self(), draw_tetromino, Tetromino,
                                       NewTetromino},
                            {NewTetromino, NewPreview, NewBoard, NewTimer};
                        _ -> {Tetromino, Preview, Board, Timer}
                    end
            end;
        false ->  % Redraw new tetromino and ghost
            case Key of 
                ?KEY_SPACE -> 
                    process_key(?ceKEY_DOWN, NewT2, Preview, Board, Pids);
                _ -> 
                    Painter ! {self(), draw_tetromino, Tetromino, NewT2},
                    {NewT2, Preview, Board, Timer}
            end
    end.

%%% lost_input/4 acts as the input loop after the user has lost
-spec lost_input(pid(), pid(), pid(), pid()) -> ok.
lost_input(GameRoom, Painter, RefreshPid, TimerPid) ->
    TimerPid ! {self(), killtimer},
    receive
        {clearrow, Rows} -> 
            Painter ! {self(), clear_other_rows, Rows},
            lost_input(GameRoom, Painter, RefreshPid, TimerPid);
        {GameRoom, game_over, Rankings} -> 
            end_game_screen(Rankings, GameRoom, Painter, RefreshPid, TimerPid);
        {_Pid, key, $q} -> quit_game(GameRoom, Painter, RefreshPid, self())
    end.

%%% end_game_screen/5 displays the rankings and waits for a space press, then
%%%                   leaves the game
-spec end_game_screen([{[integer()], pid()}], pid(), pid(), pid(), pid()) ->
      ok.
end_game_screen(Rankings, GameRoom, Painter, RefreshPid, TimerPid) ->
    TimerPid ! {self(), killtimer},
    Painter ! {self(), username},
    tetris_io:set_auto_refresh(RefreshPid, true),
    UserName = get_user(Painter),
    Painter ! {self(), end_game, Rankings},
    get_space(),
    leave_game(GameRoom, Painter, RefreshPid, UserName).

%%% leave_game/4 leaves the game and returns tot he title screen, not returning
-spec leave_game(pid(), pid(), pid(), string()) -> ok.
leave_game(GameRoom, Painter, RefreshPid, UserName) ->
    GameRoom ! {playerquit, self()},
    Painter ! {lebron, jamie},
    Painter2 = spawn_link(fun () -> painter:start(UserName) end),
    tetris_io:set_resize_recipient(RefreshPid, self()),
    {Status, RoomName} = title_screen(UserName, Painter2),
    tetris(Status, Painter2, RefreshPid, RoomName).

%%% quit_game/4 quits out of the game and returns to the title screen, not
%%%             returning
-spec quit_game(pid(), pid(), pid(), pid()) -> ok.
quit_game(GameRoom, Painter, RefreshPid, TimerPid) ->
    GameRoom ! {playerquit, self()},
    TimerPid ! {self(), killtimer},
    Painter ! {self(), username},
    UserName = get_user(Painter),
    Painter ! {lebron, jamie},
    Painter2 = spawn_link(fun () -> painter:start(UserName) end),
    tetris_io:set_auto_refresh(RefreshPid, true),
    tetris_io:set_resize_recipient(RefreshPid, self()),
    {Status, RoomName} = title_screen(UserName, Painter2),
    tetris(Status, Painter2, RefreshPid, RoomName).

%%%%%%%%%% END TETRIS GAME LOGIC %%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% MISCELLANEOUS %%%%%%%%%% 



%%% get_space/0 waits for a space key press
-spec get_space() -> ok.
get_space() ->
    receive 
        {_Pid, key, ?KEY_SPACE} -> ok;
        _ -> get_space()
    end.

%%% new_timer/2 gets the speed from the listener and spawns a new timer
%%%             function to move the piece down a the time interval.
-spec new_timer(pid(), pid()) -> pid().
new_timer(Pid, Painter) ->
    Painter ! {self(), speed},
    Speed = receive 
        {Painter, speed, S} -> S
    end,
    spawn(fun () -> timer(Pid, Speed) end).

%%% check_clear_row/3 checks whether a piece cleared any rows and, if so, sends
%%%                   sends a message to the game room
-spec check_clear_row(tetromino:tetromino(), board:board(), pid()) -> ok.
check_clear_row({Type, Rotation, Center, Cells}, Board, GameRoom) ->
    Placed = tetromino:get_all_coords({Type, Rotation, Center, Cells}),
    Sorted = lists:sort(fun ({R1, _}, {R2, _}) -> R1 < R2 end, Placed),
    Rows = lists:foldl(
        fun ({Row, _Col}, Rows) ->
            IsFilled = fun (CurrCol) -> 
                           board:is_filled(Board, Row, CurrCol) end,
            case lists:all(IsFilled, lists:seq(0, ?BOARD_WIDTH - 1))
            of
                true -> [Row | Rows];
                false -> Rows
            end
        end,
        [],
        Sorted),
    case Rows of
        [] -> ok;
        _ ->
            GameRoom ! {rowcleared, Rows, self()},
            ok
    end.

%%% get_user/1 returns the username of the current player.
-spec get_user(pid()) -> string().
get_user(Painter) ->
    receive
        {Painter, user, UserName} -> UserName
    end.



%%% next_piece/2 removes the next piece from the preview and adds another new
%%%              random piece
-spec next_piece(preview(), pid()) -> {tetromino:tetromino(), preview()}.
next_piece([Piece | Tail], GameRoom) ->
    Ret = {Piece, lists:append([Tail, [tetromino:generate({1, 5}, random)]])},
    GameRoom ! {newpiece, Piece, self()},
    Ret.



%%% timer/2 acts as the loop for the timer process with a given target PID and
%%%         time between messages. It sends the target PID timer message every
%%%         Time.
-spec timer(pid(), pos_integer()) -> ok.
timer(Pid, Time) ->
    timer:sleep(Time),
    receive
        {Pid, killtimer} -> ok
    after
        0 -> 
             Pid ! {self(), timer},
             timer(Pid, Time)
    end.


%%% get_game_width/1 returns the total game of the board given the number of
%%%                  players in the game room
-spec get_game_width(integer()) -> integer().
get_game_width(1) ->
    ?BOARD_WIDTH;
get_game_width(NumPlayers) when NumPlayers > 1 ->
    ?BOARD_WIDTH * NumPlayers + 2.

%%% clear_board_rows/2 clears all rows in board
-spec clear_board_rows(list(integer()), board:board()) -> board:board().
clear_board_rows(Rows, Board) ->
    lists:foldl(fun (R, CurrBoard) -> board:remove_row(CurrBoard, R) end,
                Board, Rows).

%%% clear_row removes the given rows from the given board
-spec clear_row(list(integer()), board:board(), pid(), tetromino:tetromino()) ->
                     board:board().
clear_row(Rows, Board, Painter, Tetromino) ->
    NewBoard = clear_board_rows(Rows, Board),
    Painter ! {self(), clear_row, NewBoard, Rows, Tetromino},
    receive
        {Painter, continue} -> ok
    end,
    NewBoard. 
