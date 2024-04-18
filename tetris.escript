#!/usr/bin/env escript
%%! -noinput -pa _build/default/lib/cecho/ebin _build/default/lib/tetris/ebin +A 50
-include_lib("cecho/include/cecho.hrl").
main(_) -> net_kernel:start(['client@vm-hw06.eecs.tufts.edu', longnames]),
           erlang:set_cookie('monster'),
    
    % io:format("~p~n", [node()]).
    tetris:initiate().

