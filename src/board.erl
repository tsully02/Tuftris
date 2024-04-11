-module(board).

-include_lib("../cecho/include/cecho.hrl").
-include_lib("tetris.hrl").

-export([create_board/3]).

create_board(Width, Height, _Color) ->
    List = list_of_arrays(Height, [], Width),
    lists:map(fun (Arr) -> array:map(fun (_, _) -> {false, ?ceCOLOR_CYAN} end, Arr) end, List).
    
    % Arr = array:new(Height),
    % array:map(fun (_, _) ->
    %     array:map(fun (_, _) ->
    %         {false, ?ceCOLOR_YELLOW}
    %     end, array:new(Width)) end, Arr).

list_of_arrays(0, List, _ArrLen) -> List;
list_of_arrays(LLen, List, ArrLen) ->
    list_of_arrays(LLen - 1, lists:append(List, [array:new(ArrLen)]), ArrLen).