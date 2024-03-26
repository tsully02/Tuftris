-module(tetris).

-include_lib("cecho/include/cecho.hrl").

-export([hello_world/0]).

hello_world() ->
%% Start application
    application:start(tetris),
    %% Set attributes
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    %% Write initial string...
    cecho:mvaddstr(0, 0, "Hello World!"),
    cecho:refresh().