-module(graphviz).
-export([digraph/1, graph/1, delete/0, add_node/1, add_edge/3, graph_server/1, to_dot/1, to_file/2,send_data/1,start/1,stop/0]).

% Building Matrix
%----------------------------
start(Number_of_Stations) ->
   Graph_server_Pid=open_Nodes(Number_of_Stations,start,[]),
   graph_server ! {build_table, Number_of_Stations},Graph_server_Pid.

stop()-> graph_server ! stop,
		timer_refresh ! stop.
%-----------------------------
send_data(Data) ->
   io:format("graphviz:send_data data is ~p~n ",[Data]),
   graph_server ! {add_connection, Data}.
   

% -- Constructor
digraph(Id) ->
   register(graph_server, spawn(?MODULE, graph_server, [{Id, {digraph, "->"}, [] ,[], []}])),whereis(graph_server).

graph(Id) ->
   register(graph_server, spawn(?MODULE, graph_server, [{Id, {graph, "--"}, [] ,[], []}])).

% -- Destructor
delete() ->
   graph_server ! stop,
   timer_refresh ! stop.
% -- Server/Dispatcher
graph_server(Graph) ->
   receive
      {add_node, Id} -> 
         graph_server(add_node(Graph, Id));

      {add_edge, NodeOne, NodeTwo,Transmission_medium} -> 
         graph_server(add_edge(Graph, NodeOne, NodeTwo,Transmission_medium));

      {to_dot, File} ->
         to_dot(Graph, File),
         graph_server(Graph);

      {to_file, File, Format} -> 
         to_file(Graph, File, Format),
         graph_server(Graph);

      {value, Pid} -> 
         Pid ! {value, Graph}, 
         graph_server(Graph);

      stop -> true ;

      {build_table, Number_of_Stations} ->
         ets:new(adjacency_matrix,[named_table,public,{read_concurrency,true}]),
         Inner_List=lists:seq(1,Number_of_Stations),
         Inner_List_Tuple =[{0,0,true}|| _X <-Inner_List],
         Big_List = [Inner_List_Tuple|| _X <-Inner_List],
         ets:insert(adjacency_matrix,{table,Big_List}),
         Pid=spawn(fun() -> refresh_graph() end),
         register(timer_refresh,Pid),
         timer_refresh ! run,
         graph_server(Graph) ;

      {add_connection, Data} ->
         [Row,Column,Medium]=Data,
         [{table,Big_List}]=ets:lookup(adjacency_matrix,table),
        
         if 
           Medium =:= none ->
             Number_of_Stations = length(Big_List),
             Inner_List=lists:seq(1,Number_of_Stations),
             New_Table_Row =[{0,0,true}|| _X <-Inner_List],
             New_Big_List = replacenth(Big_List,Row,New_Table_Row),
             ets:insert(adjacency_matrix,{table,New_Big_List}), 
             Graph_Node_Name_1= erlang:integer_to_list(Row),            
             New_Graph = delete_edge(Graph, Graph_Node_Name_1,Number_of_Stations),    
             graph_server(New_Graph) ;

           Medium =/= none ->
             Number_of_Stations = length(Big_List),
             Inner_List=lists:seq(1,Number_of_Stations),
             New_Table_Row1=[{0,0,true}|| _X <-Inner_List],
             New_Big_List1 = replacenth(Big_List,Row,New_Table_Row1),
             Table_Row = lists:nth(Row,New_Big_List1),
             New_Entry = {1,Medium,true},
             New_Table_Row2 =replacenth(Table_Row,Column,New_Entry),
             New_Big_List2 = replacenth(Big_List,Row,New_Table_Row2),
             ets:insert(adjacency_matrix,{table,New_Big_List2}),
             Graph_Node_Name_1= erlang:integer_to_list(Row),            
             New_Graph = delete_edge(Graph, Graph_Node_Name_1,Number_of_Stations),
             graph_server(New_Graph) 
         end        
   end.

% -- Methods
%============================================================================================================
%============================================================================================================

add_node(Id) -> graph_server ! {add_node, Id}.
add_edge(NodeOne, NodeTwo,Transmission_medium) -> graph_server ! {add_edge, NodeOne, NodeTwo,Transmission_medium}.
to_dot(File) -> graph_server ! {to_dot, File}.
to_file(File, Format) -> graph_server ! {to_file, File, Format}. 
   
% -- Implementation

%========================================================== 
add_node(Graph, Id) ->
   {GraphId, Type, GraphOptions, Nodes, Edges} = Graph,
   {GraphId, Type, GraphOptions, Nodes ++ [Id], Edges}.

%========================================================== 
add_edge(Graph, NodeOne, NodeTwo,Transmission_medium) ->
   {GraphId, Type, GraphOptions, Nodes, Edges} = Graph,
   case Transmission_medium of
      1 -> 
         {GraphId, Type, GraphOptions, Nodes, Edges ++ [{NodeOne, NodeTwo,Transmission_medium}]} ;
      2 ->
         {GraphId, Type, GraphOptions, Nodes, Edges ++ [{NodeOne, NodeTwo,Transmission_medium}]}
   end.

%========================================================== 

%---------------------------------------------------------
delete_edge(Graph, _NodeOne,0)->
	Graph ;
%---------------------------------------------------------
delete_edge(Graph,NodeOne,Number_of_Stations)->
   NodeTwo = erlang:integer_to_list(Number_of_Stations),
   {GraphId, Type, GraphOptions, Nodes, Edges} = Graph,
   New_Edges1 = Edges -- [{NodeOne, NodeTwo,1}],
   New_Edges2 = New_Edges1 -- [{NodeOne, NodeTwo,2}],
   delete_edge({GraphId, Type, GraphOptions, Nodes, New_Edges2}, NodeOne,Number_of_Stations-1).

%========================================================== 
to_dot(Graph, File) ->
   {GraphId, Type, _GraphOptions, Nodes, Edges} = Graph,
   {GraphType, EdgeType} = Type,
   
   % open file
   {ok, IODevice} = file:open(File, [write]),

   % print graph
   io:format(IODevice, "~s ~s {~n", [GraphType, GraphId]),

   % print nodes
   lists:foreach(
      fun(Node) ->
            io:format(IODevice, "  ~s;~n",[Node]) 
      end, 
      Nodes
   ),

   % print edges
   lists:foreach(
      fun(Edge) ->
            {NodeOne, NodeTwo,Transmission_medium} = Edge,
            case Transmission_medium of
               % wifi in red
               %-----------------------
               1 ->
                  io:format(IODevice, "  ~s ~s ~s[color=red,penwidth=3.0];~n",[NodeOne, EdgeType, NodeTwo]) ;
               % Plc in black
               %-----------------------
               2 ->  
                  io:format(IODevice, "  ~s ~s ~s;~n",[NodeOne, EdgeType, NodeTwo]) 
               end
      end, 
      Edges
   ),

   % close file
   io:format(IODevice, "}~n", []),
   file:close(IODevice).

%========================================================== 
to_file(Graph, File, Format) ->
   {A1,A2,A3} = now(),
   DotFile = lists:concat([File, ".dot-", A1, "-", A2, "-", A3]),
   to_dot(Graph, DotFile),
   DotCommant = lists:concat(["dot -T", Format, " -o", File, " ", DotFile]),
   os:cmd(DotCommant),
   file:delete(DotFile).

%============================================================================================================
%============================================================================================================
%============================================================================================================
% build graph
%============================================================================================================


% print Network Graph out
%==============================================
%==============================================

io_graph(_Matrix,0,_Matrix_Column) ->
   finished_graph;


% print Network Graph out
%-------------------------------
io_graph(Matrix,[],[]) ->
   Matrix_Size= length(Matrix),
   io_graph(Matrix,Matrix_Size,Matrix_Size);

% print Network Graph out
%-------------------------------
io_graph(Matrix,Matrix_row,0) ->
   Matrix_Size = length(Matrix),
   io_graph(Matrix,Matrix_row-1,Matrix_Size);

% print Network Graph out
%-------------------------------
io_graph(Matrix,Matrix_row,Matrix_Column) ->
   Row =lists:nth(Matrix_row,Matrix),
   Element = lists:nth(Matrix_Column,Row),
   case Element of
      {0,_,_} ->
         io_graph(Matrix,Matrix_row,Matrix_Column-1) ;

      % conected with wifi add conection
      {1,1,true}->
         Graph_Node_Name_1= erlang:integer_to_list(Matrix_row),
         Graph_Node_Name_2= erlang:integer_to_list(Matrix_Column),
         graphviz:add_edge(Graph_Node_Name_1, Graph_Node_Name_2,1),
         [{table,Big_List}]=ets:lookup(adjacency_matrix,table),
         Table_Row = lists:nth(Matrix_row,Big_List),
         New_Entry ={1,1,false},
         New_Table_Row = replacenth(Table_Row,Matrix_Column,New_Entry),
         New_Big_List = replacenth(Big_List,Matrix_row,New_Table_Row),
         ets:insert(adjacency_matrix,{table,New_Big_List}),
         io_graph(Matrix,Matrix_row,Matrix_Column-1) ;

      % conected with Plc add conection
      {1,2,true}->
         Graph_Node_Name_1= erlang:integer_to_list(Matrix_row),
         Graph_Node_Name_2= erlang:integer_to_list(Matrix_Column),
         graphviz:add_edge(Graph_Node_Name_1, Graph_Node_Name_2,2),
         [{table,Big_List}]=ets:lookup(adjacency_matrix,table),
         Table_Row = lists:nth(Matrix_row,Big_List),
         New_Entry ={1,2,false},
         New_Table_Row = replacenth(Table_Row,Matrix_Column,New_Entry),
         New_Big_List = replacenth(Big_List,Matrix_row,New_Table_Row),
         ets:insert(adjacency_matrix,{table,New_Big_List}),
         io_graph(Matrix,Matrix_row,Matrix_Column-1) ;

      % conected with wifi dont add conection
      {1,1,false}->
         io_graph(Matrix,Matrix_row,Matrix_Column-1) ;
      % conected with wifi dont add conection
      {1,2,false}->
         io_graph(Matrix,Matrix_row,Matrix_Column-1)
   end.
   

% open Nodes
%-------------------------------------------

%--------------------------
open_Nodes(0,_Stage,Pid)->
   Pid ;

%--------------------------
open_Nodes(Node_Number,Stage,Pid)->
   case Stage of 
      start ->
         New_Pid=graphviz:digraph("Network_Graph"),
         open_Nodes(Node_Number,runing,New_Pid) ;
      runing ->
         Graph_Node_Name= erlang:integer_to_list(Node_Number),
         graphviz:add_node(Graph_Node_Name),
         open_Nodes(Node_Number-1,runing,Pid)
   end.

% replace an elment in a list
%-----------------------------------
replacenth(L,Index,NewValue) -> 
 {L1,[_|L2]} = lists:split(Index-1,L),
 L1++[NewValue|L2].

%  Timer Refresh for building Graph
%-----------------------------------------
refresh_graph()->
	  receive
	  run->
      	[{table,Graph_Table}]=ets:lookup(adjacency_matrix,table),
      	io_graph(Graph_Table,[],[]),
      	graphviz:to_file("Network_Graph","png"),
      	timer:sleep(3000),
      	timer_refresh ! run,
      	refresh_graph() ;
      stop ->
      	stop
      end.

%==============================================
