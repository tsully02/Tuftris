#!/usr/bin/env escript
%%! -noinput -pa _build/default/lib/tetris/ebin +A 50
% -include_lib("cecho/include/cecho.hrl").
main(_) -> tetris:hello_world().