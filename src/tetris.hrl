-define(Rotation_T, [[{0, -1}, {-1, 0}, {0, 1}],
                     [{-1, 0}, {0, 1}, {1, 0}],
                     [{0, 1}, {1, 0}, {0, -1}], 
                     [{1, 0}, {0, -1}, {-1, 0}]]).

-define(Rotation_Left, [[{0, -1}, {0, 1}, {-1, 1}], 
                        [{-1, 0}, {1, 0}, {1, 1}], 
                        [{1, -1}, {0, -1}, {0, 1}], 
                        [{-1, -1}, {-1, 0}, {1, 0}]]).

-define(Rotation_Right, [[{-1, -1}, {0, -1}, {0, 1}], 
                         [{-1, 0}, {-1, 1}, {1, 0}], 
                         [{0, -1}, {0, 1}, {1, 1}], 
                         [{-1, 0}, {1, -1}, {1, 0}]]).

-define(Rotation_Square, [[{0, -1}, {-1, 0}, {-1, -1}],
                          [{0, -1}, {-1, 0}, {-1, -1}],
                          [{0, -1}, {-1, 0}, {-1, -1}],
                          [{0, -1}, {-1, 0}, {-1, -1}]]).

-define(Rotation_Zigz, [[{-1, -1}, {-1, 0}, {0, 1}],
                        [{1, 0}, {0, 1}, {-1, 1}],
                        [{0, -1}, {1, 0}, {1, 1}], 
                        [{1, -1}, {0, -1}, {-1, 0}]]).

-define(Rotation_Zags, [[{0, -1}, {-1, 0}, {-1, 1}],
                        [{-1, 0}, {0, 1}, {1, 1}],
                        [{1, -1}, {1, 0}, {0, 1}], 
                        [{-1, -1}, {0, -1}, {1, 0}]]).

-define(Rotation_Line, [[{0, -2}, {0, -1}, {0, 1}],
                        [{-1, 0}, {1, 0}, {2, 0}],  
                        [{0, -1}, {0, 1}, {0, 2}], 
                        [{-2, 0}, {-1, 0}, {1, 0}]]).

-define(bigboy,        [[{0, -5}, {0, -4}, {0, -3}, {0, -2}, {0, -1}, {0, 1}, {0, 2}, {0, 3}, {0, 4}],
                        [{0, -5}, {0, -4}, {0, -3}, {0, -2}, {0, -1}, {0, 1}, {0, 2}, {0, 3}, {0, 4}],  
                        [{0, -5}, {0, -4}, {0, -3}, {0, -2}, {0, -1}, {0, 1}, {0, 2}, {0, 3}, {0, 4}], 
                        [{0, -5}, {0, -4}, {0, -3}, {0, -2}, {0, -1}, {0, 1}, {0, 2}, {0, 3}, {0, 4}]]).

-define(Rotation_Line_Centers, [{1, -1},
                               {-1, 1}]).

-define(GOLD, 178).
-define(SILVER, 244).
-define(BRONZE, 130).
-define(GOLD_TEXT, 15).
-define(SILVER_TEXT, 16).
-define(BRONZE_TEXT, 17).
-define(KEY_RESIZE, 410).
-define(KEY_SPACE, 32).

-define(BOARD_WIDTH, 10).
-define(BOARD_HEIGHT, 20).

-define(TITLESCR_WIDTH, 42).
-define(TITLESCR_HEIGHT, 20).
-define(LOGO_WIDTH, 40).
-define(CLEAR_ROW_SLEEP, 40).

% 240
-define(BORDER_COLOR, 245).
-define(BACKGROUND_COLOR, 234).  % 234
-define(SCREEN_BGD_COLOR, 234).  % 234
-define(TITLE_BGD_COLOR, ?ceCOLOR_WHITE).
-define(GHOST_COLOR, 250). % refers to the color pair

-define(SERVER_NODE, 't@vm-hw05.eecs.tufts.edu').

-define(TITLE_MSG, ["Press:", "1 - Single player", "2 - Create a multiplayer room", "3 - Join a multiplayer room", "q - Quit"]).

% -define(KEYBINDS, ["[c]: Swap piece     [up]: Rotate clockwise  [space]: Hard drop   [z]: Rotate counter clockwise",
%                    "[left]: Move left   [down]: Soft drop       [right]: Move right  [q]: Quit game               "]).
% No swap for now
-define(KEYBINDS, ["[up]: Rotate clockwise  [space]: Hard drop   [z]: Rotate counter clockwise                ",
                   "[left]: Move left       [down]: Soft drop    [right]: Move right            [q]: Quit game"]).

% -define(KEYBINDS, ["[C]: Swap piece  [↑]: Rotate clockwise  [Space]: Hard drop  [Z]: Rotate counter clockwise",
%                    "[←]: Move left   [↓]: Soft drop         [→]: Move right     [Q]: Quit game               "]).

-define(TITLE_LOGO, ["[][][][][]   []    []   [][][][]   [][][][][]   [][][][]   [][][][][]   [][][][]",
                     "    []       []    []   []             []       []    []       []       []      ",
                     "    []       []    []   [][][]         []       [][][][]       []       [][][][]",
                     "    []       []    []   []             []       []  []         []             []",
                     "    []       [][][][]   []             []       []    []   [][][][][]   [][][][]"]).

-define(T_COLOR, 92).
-define(SQUARE_COLOR, 3).
-define(LEFT_COLOR, 203).
-define(RIGHT_COLOR, 4).
-define(ZIGZ_COLOR, 1).
-define(ZAGS_COLOR, 2).
-define(LINE_COLOR, 39).
