-module(foo).

-export([test/0]).

bar(Eater) ->
	case Eater of
		{Pid,OrderNum,Order} ->
			io:format("EATER: ~p ~p ~p~n",[Pid,OrderNum,Order])
	end,
	ok.

test() ->
	NumTables = 5,
	register(restaurant,semaphore:make_semaphore(NumTables)),
	OrderNum = 1,
	Order = [burger,fries,coke],
	Eater = {self(),OrderNum,Order},
	bar(Eater),
	ok.