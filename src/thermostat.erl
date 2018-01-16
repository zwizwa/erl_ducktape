-module(thermostat).
-export([loop/2]).

loop({temp, T},
     #{ relay    := true, 
        setpoint := {_,Max}} = State) ->
    Active = T < Max,
    maps:put(relay, Active, State);

loop({temp, T},
     #{ relay    := false, 
        setpoint := {Min,_}} = State) ->
    Active = T < Min,
    maps:put(relay, Active, State).



%% erlang:binary_to_float(hd(re:split(os:cmd("temper-poll -f"),"\n"))).
