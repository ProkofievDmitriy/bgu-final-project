-module(trytry).
-author("admin").


-export([start/0]).

start()-> receive
	  after 5000 ->
        	io:format("hallo world~n"), start()
           end.

