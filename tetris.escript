#!/usr/bin/env escript
%%! -noinput -pa _build/default/lib/cecho/ebin _build/default/lib/tetris/ebin +A 50
-include_lib("cecho/include/cecho.hrl").
main(_) -> tetris:initiate().

