-module(semaphore). 
-export([make_semaphore/1, put/1, take/1, test/0]). 

% create a semaphore initialized to have N permits
make_semaphore(N) -> 
    S = spawn(fun() -> sem_loop() end),
    loop(0,N,fun (_) -> put(S) end),
    S.

sem_loop() -> 
    receive 
	{From, Id, take} ->
	    receive 
		put -> 
		    From ! Id,
		    sem_loop()
	    end
    end.

% take a permit from the semaphore
take(Sem) -> 
    Id = make_ref(),
    Sem ! {self(), Id, take},
    receive 
        Id -> ok
    end.

% put a permit into a semaphore
put(Sem) ->
    Sem ! put,
    ok.

% helper fun
loop(N, N, _) -> ok;
loop(I, N, F) when I < N -> 
    F(I),
    loop(I+1, N, F).

% Unit test
test() ->
    S = make_semaphore(0),
    spawn(fun() -> 
		  put(S), 
		  ok = take(S),
		  ok = take(S),
		  put(S)
	  end),
    put(S),
    ok = take(S),
    ok.
