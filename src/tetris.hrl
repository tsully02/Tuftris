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

-define(Rotation_Line_Centers, [{1, -1},
                               {-1, 1}]).

-define(KEY_RESIZE, 410).
-define(KEY_SPACE, 32).

-define(BOARD_WIDTH, 10).
-define(BOARD_HEIGHT, 20).

-define(TITLESCR_WIDTH, 42).
-define(TITLESCR_HEIGHT, 20).
-define(LOGO_WIDTH, 40).

% 240
-define(BORDER_COLOR, 245).
-define(BACKGROUND_COLOR, 234).  % 234
-define(SCREEN_BGD_COLOR, 234).  % 234
-define(GHOST_COLOR, 250). % refers to the color pair

-define(TITLE_MSG, ["Press:", "1 - Single player", "2 - Create a multiplayer room", "3 - Join a multiplayer room", "q - quit"]).

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