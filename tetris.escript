#!/usr/bin/env escript
%%! -noinput -setcookie monster -pa _build/default/lib/cecho/ebin _build/default/lib/tetris/ebin +A 50
-include_lib("cecho/include/cecho.hrl").
main([ClientLongName]) -> 
    net_kernel:start([list_to_atom(ClientLongName), longnames]),    
            % io:format("~p~n", [node()]).
            tetris:initiate();
main(_) ->
    io:format("Usage: ./tetris.escript <longname>~n").
