-module(tetris).
-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([initiate/1, timer/2]).

initiate(UserName) ->
    Listener = spawn_link(fun () -> listener(UserName) end),
    {_KeyPid, RefreshPid, _MaxRow, _MaxCol} = tetris_io:init(),
    % Num = add_horiz_line_c(30, 30, 11000, 1000),
    % io:format("~p~n", [Num]),
    % cecho:refresh(),
    % receive_space("ope"),
    Status = start_screen(UserName, Listener),
    % Disable auto refresh during the game itself; leave it up to the main client process
    tetris_io:set_auto_refresh(RefreshPid, false),
    tetris_io:set_resize_recipient(RefreshPid, Listener),
    tetris(Status, Listener).

% add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
% add_horiz_line_c(Row, Col, Length, ColorNum) ->
%     cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
%     cecho:attron(?ceCOLOR_PAIR(ColorNum)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).

tetris(quit, _) ->
    tetris_io:stop();
tetris({game, {GameRoom, NumPlayers}}, Listener) ->
    Status = wait_to_start(GameRoom),
    start_game(GameRoom, NumPlayers, Listener, Status).

start_game(_, _, _, quit) -> ok;
start_game(GameRoom, NumPlayers, Listener, ok) ->
    % io:format("LENGTH: ~p~n", [length(Players)]),
    % timer:sleep(2000),
    Win = tetris_io:calc_game_win_coords(?BOARD_WIDTH * NumPlayers + 2, ?BOARD_HEIGHT),
    Board = board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, bg),
    TimerPid = new_timer(self(), 1000),
    T = tetromino:generate({1, 5}, GameRoom),
    GameRoom ! {newpiece, T, self()},
    tetris_io:paint_screen(border),
    Preview = [tetromino:generate({1, 5}, GameRoom), tetromino:generate({1, 5}, GameRoom), tetromino:generate({1, 5}, GameRoom)],
    Listener ! {self(), winboardprev, Win, Board, Preview},
    Listener ! {self(), draw},
    Listener ! draw,
    % tetris_io:draw_board(Board, Win),
    % tetris_io:draw_board(Board, Win),
    Listener ! {self(), draw_tetromino, T, T},
    % tetris_io:draw_tetromino(T, Win),
    Listener ! {self(), draw_preview, Preview},
    % tetris_io:draw_preview(Preview, Win, bg),
    Listener ! {self(), draw_tetromino, T, T},
    wait_for_input(T, Preview, Win, Board, TimerPid, GameRoom, Listener).

new_timer(Pid, Time) ->
    spawn(fun () -> timer(Pid, Time) end).

wait_to_start(GameRoom) ->
    receive
        {_Pid, start} -> ok;
        {_Pid, key, $q} ->
            tetris_io:stop(),
            io:format("Thanks for playing!~n"),
            quit;
        _ -> wait_to_start(GameRoom)
    end.

title_screen(Win, UserName, Listener) ->
    tetris_io:draw_title_screen(Win, ?TITLE_MSG),
    Status = title_screen_keyboard_loop(UserName, Win, Listener),
    tetris_io:paint_screen(scrbg),
    Status.

get_num_players(Msg) -> 
    receive
        {_Pid, key, Key} when Key =< $5, Key >= $2 -> 
            Key - $0;
        {_Pid, key, $q} -> 
            0;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            get_num_players(Msg);
        {_Pid, key, _} -> get_num_players(Msg)
    end.

create_multi_room(UserName, Win, Listener) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    NumPlayersMsg = ["Enter the number of players (2-5)"],
    tetris_io:draw_title_screen(Win, NumPlayersMsg),
    NumPlayers = get_num_players(NumPlayersMsg),
    case NumPlayers of
        0 -> title_screen_keyboard_loop(UserName, Win, Listener);
        _ ->  GameRoom = server:create_room('a@vm-hw05.eecs.tufts.edu', RoomName, UserName, NumPlayers, Listener),
            case GameRoom of 
                already_exists -> 
                    Msg = lists:append(["Room ", RoomName, " already exists!"]),
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win, Listener);
                _ -> {game, {GameRoom, NumPlayers}}
            end
    end.

join_multi_room(UserName, Win, Listener) ->
    tetris_io:draw_title_screen(Win, ["Enter a room name:"]),
    RoomName = tetris_io:text_box(Win, 15, 14),
    GameInfo = server:join_room('a@vm-hw05.eecs.tufts.edu', RoomName, UserName, Listener),
    case GameInfo of 
        room_full -> 
            Msg = lists:append(["Room ", RoomName, " is full, sorry :("]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win, Listener);
        no_such_room -> 
            Msg = lists:append(["Room ", RoomName, " does not exist!"]),
            error_title(Win, Msg),
            title_screen_keyboard_loop(UserName, Win, Listener);
        _ -> {game, GameInfo}
    end.

error_title(Win, Msg) ->
    FullMsg = [Msg, "Press [Space] to continue"],
    tetris_io:draw_title_screen(Win, FullMsg),
    receive_space(FullMsg),
    tetris_io:draw_title_screen(Win, ?TITLE_MSG).

receive_space(Msg) -> 
    receive
        {_Pid, key, ?KEY_SPACE}-> ok;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, Msg),
            receive_space(Msg);
        _ -> receive_space(Msg)
    end.

title_screen_keyboard_loop(UserName, Win, Listener) ->
    receive
        {_Pid, key, $1} -> 
            GameRoom = server:create_room('a@vm-hw05.eecs.tufts.edu', UserName, UserName, 1, Listener),
            case GameRoom of 
                already_exists -> 
                    Msg = "You are already playing a solo game! :P",
                    error_title(Win, Msg),
                    title_screen_keyboard_loop(UserName, Win, Listener);
                _ -> {game, {GameRoom, 1}}
            end;
        {_Pid, key, $2} -> 
            % io:format("messaging server...~n"),
            create_multi_room(UserName, Win, Listener);
        {_Pid, key, $3} -> 
            join_multi_room(UserName, Win, Listener);
        {_Pid, key, $q} -> quit;
        {_Pid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
            tetris_io:draw_title_screen(NewWin, ?TITLE_MSG),
            title_screen_keyboard_loop(UserName, NewWin, Listener);
        % {_Pid, key, $t} -> tboxtest;
        {_Pid, key, _} -> title_screen_keyboard_loop(UserName, Win, Listener)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Piece Tuple Notation:
%%% 
%%%     {Type, Rotation, Center, Cells}
%%% 
%%% Type: atom denoting the type of piece (t, square, left, right, zig, zag, line)
%%% Rotation: integer between 0 and 3 denoting the direction (0 is original 
%%%            direction, 1 is 90° clockwise, 2 is 180° clockwise, etc.)
%%% Center: "Center" index of piece (not always the actual center)
%%% Cells: Location of cells, relative to the center (add the cell values to the 
%%%             center coords to get the cell coords)
%%%
%%% Right now the program places a piece in the middle of the screen, and supports 
%%% the following keyboard inputs (see wait_for_input/1)
%%%     q: quit
%%%     Up arrow: rotate clockwise
%%%     Z: rotate counter clockwise
%%%     Right arrow: move right
%%%     Left arrow: move left
%%%     Down arrow: move down

check_clear_row({Type, Rotation, Center, Cells}, Board, GameRoom) ->
    Placed = tetromino:get_all_coords({Type, Rotation, Center, Cells}),
    Sorted = lists:sort(fun ({R1, _}, {R2, _}) -> R1 < R2 end, Placed),
    Rows = lists:foldl(
        fun ({Row, _Col}, Rows) ->
            case lists:all(fun (CurrCol) -> board:is_filled(Board, Row, CurrCol) end, lists:seq(0, ?BOARD_WIDTH - 1))
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

clear_board_rows(Rows, Board) ->
    lists:foldl(fun (R, CurrBoard) -> board:remove_row(CurrBoard, R) end, Board, Rows).

clear_row(Rows, Board, Listener, Tetromino) ->
    NewBoard = clear_board_rows(Rows, Board),
    Listener ! {self(), clear_row, NewBoard, Rows, Tetromino},
    receive
        {Listener, continue} -> ok
    end,
    NewBoard.

process_key(Key, Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener) -> 
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
                1 -> blockedOut();
                _ ->
                    case Key of 
                        ?ceKEY_DOWN -> 
                            NewBoard = board:place_piece(Board, Tetromino),
                            Listener ! {self(), new_board, NewBoard},
                            GameRoom ! {placepiece, Tetromino, self()},
                            Listener ! {self(), draw},
                            % tetris_io:draw_board(NewBoard, Win),
                            TimerPid ! {self(), kill},
                            NewTimerPid = new_timer(self(), 1000),
                            check_clear_row(Tetromino, NewBoard, GameRoom),
                            {NewTetromino, NewPreview} = next_piece(Preview, GameRoom),
                            Listener ! {self(), draw_preview, NewPreview},
                            % tetris_io:draw_preview(NewPreview, Win, bg),
                        
                            % tetris_io:delete_tetromino(Tetromino, Win, NewBoard),
                            
                            % tetris_io:draw_tetromino(NewTetromino, Win),
                            Listener ! {self(), draw_tetromino, Tetromino, NewTetromino},
                            {Win, NewTetromino, NewPreview, NewBoard, NewTimerPid};
                        _ -> {Win, Tetromino, Preview, Board, TimerPid}
                    end
            end;
        false ->  % Redraw new tetromino and ghost
            case Key of 
                ?KEY_SPACE -> 
                    tetris_io:delete_tetromino(Tetromino, Win, Board),
                    process_key(?ceKEY_DOWN, NewT2, Preview, Win, Board, TimerPid, GameRoom, Listener);
                _ -> 
                    % ResultGhost = tetromino:get_ghost(NewT2, Board),
                    % tetris_io:delete_tetromino(Ghost, Win, Board),
                    % tetris_io:draw_ghost(ResultGhost, Win, Board),
                    % tetris_io:delete_tetromino(Tetromino, Win, Board),
                    Listener ! {self(), draw_tetromino, Tetromino, NewT2},
                    % tetris_io:draw_tetromino(NewT2, Win),
                    {Win, NewT2, Preview, Board, TimerPid}
            end
    end.

wait_for_input(Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener) ->
    % io:format("Timer: ~p~n", [TimerPid]),
    receive
        {TimerPid, timer} ->
            case process_key(?ceKEY_DOWN, Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener) of
                blocked -> 
                    GameRoom ! {playerlost, self()},
                    wait_for_input(Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener);
                {NewWin, NewTetromino, NewPreview, NewBoard, NewTimerPid} ->
                           wait_for_input(NewTetromino, NewPreview, NewWin, NewBoard, NewTimerPid, GameRoom, Listener)
            end;
        {GameRoom, game_over} ->
            tetris_io:stop(),
            io:format("Thanks for playing!~n"),
            ok;
        {_Pid, key, $q} -> 
            GameRoom ! {playerquit, self()},
            tetris_io:stop(),
            io:format("Thanks for playing!~n"),
            ok;
        {_Pid, key, $l} -> 
            NewTetromino = tetromino:generate({1, 5}, line),
            Listener ! {self(), draw_tetromino, Tetromino, NewTetromino},
            wait_for_input(NewTetromino, Preview, Win, Board, TimerPid, GameRoom, Listener);
        {_Pid, key, Key} ->
            case process_key(Key, Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener) of
                blocked -> 
                    GameRoom ! {playerlost, self()},
                    wait_for_input(Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener);
                {NewWin, NewTetromino, NewPreview, NewBoard, NewTimerPid} ->
                    wait_for_input(NewTetromino, NewPreview, NewWin, NewBoard, NewTimerPid, GameRoom, Listener)
            end;
        {clearrow, Rows} -> 
            % io:format("row cleared~n"),
            NewBoard = clear_row(Rows, Board, Listener, Tetromino),
            % NewGhost = tetromino:get_ghost(Tetromino, NewBoard),
            % tetris_io:draw_ghost(NewGhost, Win, NewBoard),
            % tetris_io:draw_tetromino(Tetromino, Win),
            % Listener ! {self(), new_board, NewBoard},
            % Listener ! {self(), draw_tetromino, Tetromino, Tetromino},
            % cecho:refresh(),
            wait_for_input(Tetromino, Preview, Win, NewBoard, TimerPid, GameRoom, Listener);
        {newpiece, _PInfo, _T} -> 
            % io:format("Player ~p got a new piece!", [PInfo]),
            % send message to pid that controls other player's boards???
            wait_for_input(Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener);
        {placepiece, _PInfo, _T} -> 
            wait_for_input(Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener);
        {_Pid, timer} ->
            wait_for_input(Tetromino, Preview, Win, Board, TimerPid, GameRoom, Listener);
        {_Pid, resize} ->
                    NewWin = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
                    tetris_io:paint_screen(border),
                    Listener ! draw,
                    % tetris_io:draw_board(Board, NewWin),
                    Listener ! {self(), draw_preview, Preview},
                    % tetris_io:draw_preview(Preview, NewWin, bg),
                    % ResultGhost = tetromino:get_ghost(Tetromino, Board),
                    % tetris_io:draw_ghost(ResultGhost, NewWin, Board),
                    % tetris_io:draw_tetromino(Tetromino, NewWin),
                    Listener ! {self(), draw_tetromino, Tetromino, Tetromino},
                    wait_for_input(Tetromino, Preview, NewWin, Board, TimerPid, GameRoom, Listener);

        Other ->
            io:format("Client Unexpected msg: ~p~n", [Other])
    end.

next_piece([Piece | Tail], GameRoom) ->
    Ret = {Piece, lists:append([Tail, [tetromino:generate({1, 5}, GameRoom)]])},
    GameRoom ! {newpiece, Piece, self()},
    Ret.

timer(Pid, Time) ->
    timer:sleep(Time),
    receive
        {Pid, kill} -> ok
    after
        0 -> 
             Pid ! {self(), timer},
             timer(Pid, Time)
    end.

% TODO: Implement BlockedOut behavior
blockedOut() ->
    blocked.
    
start_screen(UserName, Listener) ->
    TitleWin = tetris_io:calc_game_win_coords(?TITLESCR_WIDTH, ?TITLESCR_HEIGHT),
    title_screen(TitleWin, UserName, Listener).


%%% 
%%% Players list:
%%% [{{Name, Pid, Listener}, Board} | Players]
listener(UserName) ->
    receive
        {GameRoom, players, Players} -> 
            {Win, Board, Preview} = receive
                {_Pid, winboardprev, W, B, P} -> {W, B, P}
            end,
            NewPlayers = lists:keydelete(UserName, 1, Players),
            Self = {lists:keyfind(UserName, 1, Players), Board},
            PlayersList = lists:map(fun (Player) -> {Player, board:create(?BOARD_WIDTH, ?BOARD_HEIGHT, bg)} end, NewPlayers),
            % GameRoom ! {playerlist, PlayersList},
            paint_players(Self, Preview, PlayersList, Win, GameRoom)
    end.

% PlayerNum start at 0 (first other player)
get_player_win(PlayerNum, {Y, X, _, H}) ->
    W = ?BOARD_WIDTH * 2,
    {Y, X + W + 2 + (PlayerNum * W), W, H}.

draw_boards(PlayersList, {Y, X, Width, Height}, GameRoom) ->
    lists:foldl(fun ({Player, Board}, Col) ->
                    tetris_io:draw_board(Board, {Y, Col, Width, Height}),
                    GameRoom ! {player, Player, col, Col},
                    Col + (?BOARD_WIDTH * 2)
                end, X + (?BOARD_WIDTH * 2) + 2, PlayersList).

get_board(Players, PlayerInfo) -> get_board_r(Players, PlayerInfo, 0).


get_board_r([], _PlayerInfo, _Idx) -> {error, doesnotexist};
get_board_r([{{_, PlayerPid, _}, Board} | _T], PlayerPid, Idx) -> 
    {Idx, Board};
get_board_r([_ | T], PlayerPid, Idx) ->
    get_board_r(T, PlayerPid, Idx + 1).

update_board([], _PlayerPid, _NewBoard) -> [];
update_board([{PlayerInfo={_, PlayerPid, _}, _Board} | T], PlayerPid, NewBoard) -> 
    [{PlayerInfo, NewBoard} | T];
update_board([H | T], PlayerInfo, NewBoard) ->
    [H | update_board(T, PlayerInfo, NewBoard)].

clear_all_boards([], Rows) -> [];
clear_all_boards([{Player, Board} | T], Rows) ->
    [{Player, clear_board_rows(Rows, Board)} | clear_all_boards(T, Rows)].


%%% 
%%% Listener needs to:
%%% initiate map of player pids to names/boards/windows
%%%     - get message from GameRoom each time a player joins
%%% receive messages for other players' placed pieces
%%%     - then change that player's board
%%%fdd

paint_players(Self={{Name, Pid, Listener}, Board}, Preview, Players, Win, GameRoom) ->
    % Self = {{_Name, Pid, _Listener}, Board},
    Me = {Name, Pid, Listener},
    receive
        {lebron, jamie} -> ok; % Goated with house sauce, kid perhaps
        {Pid, draw} ->
            tetris_io:draw_board(Board, Win),
            paint_players(Self, Preview, Players, Win, GameRoom);
        {Pid, new_board, NewBoard} -> 
            NewSelf = {Me, NewBoard},
            tetris_io:draw_board(NewBoard, Win),
            cecho:refresh(),
            paint_players(NewSelf, Preview, Players, Win, GameRoom);
        {Pid, clear_row, NewBoard, Rows, Tetromino} ->
            NewSelf = {Me, NewBoard},
            NewPlayers = clear_all_boards(Players, Rows),
            FirstWin = get_player_win(0, Win),
            tetris_io:animate_clear_row(Rows, Win, ?BOARD_WIDTH),
            tetris_io:draw_board(NewBoard, Win),
            Ghost = tetromino:get_ghost(Tetromino, NewBoard),
            tetris_io:draw_ghost(Ghost, Win, NewBoard),
            tetris_io:draw_tetromino(Tetromino, Win),
            cecho:refresh(),
            tetris_io:animate_clear_row(Rows, FirstWin, ?BOARD_WIDTH * length(Players)),
            draw_boards(NewPlayers, Win, GameRoom),
            % tetris_io:draw_board(NewBoard, Win),
            cecho:refresh(),
            Pid ! {self(), continue},
            paint_players(NewSelf, Preview, NewPlayers, Win, GameRoom);
        {Pid, draw_tetromino, OldT, NewT} ->
            OldG = tetromino:get_ghost(OldT, Board),
            tetris_io:delete_tetromino(OldG, Win, Board),
            tetris_io:delete_tetromino(OldT, Win, Board),
            NewG = tetromino:get_ghost(NewT, Board),
            tetris_io:draw_ghost(NewG, Win, Board),
            tetris_io:draw_tetromino(NewT, Win),
            paint_players(Self, Preview, Players, Win, GameRoom);
        {Pid, draw_preview, NewPreview} ->
            tetris_io:draw_preview(NewPreview, Win, bg),
            paint_players(Self, NewPreview, Players, Win, GameRoom);
        draw -> 
            tetris_io:draw_board(Board, Win),
            draw_boards(Players, Win, GameRoom),
            paint_players(Self, Preview, Players, Win, GameRoom);
        {GameRoom, placepiece, PlayerPid, T} -> 
            GameRoom ! {gotmessage, PlayerPid, placepiece},
            {Idx, PlayerBoard} = get_board(Players, PlayerPid),
            NewBoard = board:place_piece(PlayerBoard, T),
            BoardWin = get_player_win(Idx, Win),
            tetris_io:draw_board(NewBoard, BoardWin),
            NewPlayers = update_board(Players, PlayerPid, NewBoard),
            paint_players(Self, Preview, NewPlayers, Win, GameRoom);
        % {newpiece, Player, }
        {_RefreshPid, resize} ->
            NewWin = tetris_io:calc_game_win_coords(?BOARD_WIDTH, ?BOARD_HEIGHT),
            tetris_io:paint_screen(border),
            tetris_io:draw_preview(Preview, NewWin, bg),
            draw_boards([Self | Players], NewWin, GameRoom),
            paint_players(Self, Preview, Players, NewWin, GameRoom);
        % {newplayer, Player}  -> paint_players(Self, [Player | Players], Win, GameRoom)
        Msg ->
            GameRoom ! {unexpected_msg, self(), Msg},
            paint_players(Self, Preview, Players, Win, GameRoom)
    end.