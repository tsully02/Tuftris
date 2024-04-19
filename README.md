```
███╗   ███╗██╗   ██╗██╗  ████████╗██╗██████╗ ██╗      █████╗ ██╗   ██╗███████╗██████╗ 
████╗ ████║██║   ██║██║  ╚══██╔══╝██║██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗
██╔████╔██║██║   ██║██║     ██║   ██║██████╔╝██║     ███████║ ╚████╔╝ █████╗  ██████╔╝
██║╚██╔╝██║██║   ██║██║     ██║   ██║██╔═══╝ ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗
██║ ╚═╝ ██║╚██████╔╝███████╗██║   ██║██║     ███████╗██║  ██║   ██║   ███████╗██║  ██║
╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═╝   ╚═╝╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
                  ████████╗███████╗████████╗██████╗ ██╗███████╗
                  ╚══██╔══╝██╔════╝╚══██╔══╝██╔══██╗██║██╔════╝
                     ██║   █████╗     ██║   ██████╔╝██║███████╗
                     ██║   ██╔══╝     ██║   ██╔══██╗██║╚════██║
                     ██║   ███████╗   ██║   ██║  ██║██║███████║
                     ╚═╝   ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝╚══════╝

```

Build
-----

    $ rebar3 compile

Run 
-----
    $ ./tetris





Commented out code:


% move tetromino to specified coordinate (will be used for implementing space)
% move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) when Row < 0 ->
%     {Type, Rotation, {0, Col}, Cells};
% move_tetromino({Row, Col}, {Type, Rotation, _Center, Cells}) when Col < 0 ->
%     {Type, Rotation, {Row, 0}, Cells};


% add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
% add_horiz_line_c(Row, Col, Length, ColorNum) ->
%     cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
%     cecho:attron(?ceCOLOR_PAIR(ColorNum)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).

% add_horiz_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
% add_horiz_line(Row, Col, Length) when Length rem 2 == 0 ->
%     cecho:attron(?ceCOLOR_PAIR(60)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line(Row, Col + 1, Length - 1);
% add_horiz_line(Row, Col, Length) when Length rem 2 == 1 ->
%     cecho:attron(?ceCOLOR_PAIR(8)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line(Row, Col + 1, Length - 1).

% add_check_horiz_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
% add_check_horiz_line(Row, Col, Length) when Length rem 2 == 0 ->
%     cecho:attron(?ceCOLOR_PAIR(60)),
%     draw_square({Row, Col}),
%     add_check_horiz_line(Row, Col + 2, Length - 1);
% add_check_horiz_line(Row, Col, Length) when Length rem 2 == 1 ->
%     cecho:attron(?ceCOLOR_PAIR(8)),
%     draw_square({Row, Col}),
%     add_check_horiz_line(Row, Col + 2, Length - 1).

% add_vert_line(EndRow, EndCol, 0) -> {EndRow, EndCol};
% add_vert_line(Row, Col, Length) ->
%     cecho:mvaddch(Row, Col, $|),
%     add_vert_line(Row + 1, Col, Length - 1).

% add_horiz_line_c(_, _, 0, ColorNum) -> ColorNum;
% add_horiz_line_c(Row, Col, Length, ColorNum) ->
%     cecho:init_pair(ColorNum, ?ceCOLOR_BLACK, ColorNum),
%     cecho:attron(?ceCOLOR_PAIR(ColorNum)),
%     cecho:mvaddch(Row, Col, $ ),
%     add_horiz_line_c(Row, Col + 1, Length - 1, ColorNum + 1).

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