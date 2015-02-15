-module(test).
-export([test_public_basic/0]).

test_public_basic() ->
	test_any(200,100,50,25).

test_any(Eaters,Tables,Cooks,Machcap) ->
try simulation:start(Eaters,Tables,Cooks,Machcap)
catch 
    _:_-> io:put_chars(standard_error, "Caught an exception")
end.
