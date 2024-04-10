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

-define(BOARD_WIDTH, 10).
-define(BOARD_HEIGHT, 20).

-define(BACKGROUND_COLOR, 234).

%%% old macros with width 2
% -define(Rotation_T, [[{0, 2}, {1, 0}, {0, -2}], 
%                      [{1, 0}, {0, -2}, {-1, 0}], 
%                      [{0, -2}, {-1, 0}, {0, 2}], 
%                      [{-1, 0}, {0, 2}, {1, 0}]]).

% -define(Rotation_Left, [[{0, -2}, {0, 2}, {-1, 2}], 
%                         [{-1, 0}, {1, 0}, {1, 2}], 
%                         [{1, -2}, {0, -2}, {0, 2}], 
%                         [{-1, -2}, {-1, 0}, {1, 0}]]).

% -define(Rotation_Right, [[{-1, -2}, {0, -2}, {0, 2}], 
%                          [{-1, 0}, {-1, 2}, {1, 0}], 
%                          [{0, -2}, {0, 2}, {1, 2}], 
%                          [{-1, 0}, {1, -2}, {1, 0}]]).

% -define(Rotation_Square, [[{0, 2}, {1, 0}, {1, 2}],
%                           [{0, 2}, {1, 0}, {1, 2}],
%                           [{0, 2}, {1, 0}, {1, 2}],
%                           [{0, 2}, {1, 0}, {1, 2}]]).

% -define(Rotation_Zigz, [[{0, -2}, {1, 0}, {1, 2}], 
%                         [{1, -2}, {0, -2}, {-1, 0}], 
%                         [{-1, -2}, {-1, 0}, {0, 2}], 
%                         [{1, 0}, {0, 2}, {-1, 2}]]).

% -define(Rotation_Zags, [[{1, -2}, {1, 0}, {0, 2}], 
%                         [{-1, -2}, {0, -2}, {1, 0}], 
%                         [{0, -2}, {-1, 0}, {-1, 2}], 
%                         [{-1, 0}, {0, 2}, {1, 2}]]).

% -define(Rotation_Line, [[{0, -4}, {0, -2}, {0, 2}],
%                         [{-1, 0}, {1, 0}, {2, 0}],  
%                         [{0, -2}, {0, 2}, {0, 4}], 
%                         [{-2, 0}, {-1, 0}, {1, 0}]]).

% -define(Rotation_Line_Centers, [{1, -2},
%                                {-1, 2}]).