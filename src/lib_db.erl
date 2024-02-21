%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_db).     
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 dynamic_db_init/1
	]).

-export([
	 
	 dynamic_install_start/1,
	 dynamic_install/2,
	 load_textfile/1
	 ]).

%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
dynamic_db_init([])->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    %% First node - create the needed tables
    io:format("First node ~p~n",[node()]);
   % db_config:start();
dynamic_db_init(Nodes) ->
   io:format("not first node ~p~n",[Nodes]),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    add_extra_nodes(Nodes).

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
-define(WAIT_FOR_TABLES,5000).
add_extra_nodes([Node|T])->
    case mnesia:change_config(extra_db_nodes,[Node]) of
	{ok,[Node]}->
	    mnesia:add_table_copy(schema,node(),ram_copies),
	    %% applicationa tables
	    Tables=rpc:call(Node,mnesia,system_info,[tables]),	 
	    [mnesia:add_table_copy(Table,node(),ram_copies)||Table<-Tables,
							     Table/=schema],
	    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES);
	_ ->
	    add_extra_nodes(T)
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
dynamic_install_start(IntialNode)->
    stopped=rpc:call(IntialNode,mnesia,stop,[]),
    {aborted,{node_not_running,IntialNode}}=rpc:call(IntialNode,mnesia,del_table_copy,[schema,IntialNode]),
    ok=rpc:call(IntialNode,mnesia,delete_schema,[[IntialNode]]),
    ok=rpc:call(IntialNode,mnesia,create_schema,[[IntialNode]]),
    ok=rpc:call(IntialNode,mnesia,start,[]).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
dynamic_install([],_IntialNode)->
    ok;
dynamic_install([NodeToAdd|T],IntialNode)->
    stopped=rpc:call(NodeToAdd,mnesia,stop,[]),
    {aborted,{node_not_running,NodeToAdd}}=rpc:call(NodeToAdd,mnesia,del_table_copy,[schema,IntialNode]),
    ok=rpc:call(NodeToAdd,mnesia,delete_schema,[[NodeToAdd]]),
    ok=rpc:call(NodeToAdd,mnesia,start,[]),
    case rpc:call(IntialNode,mnesia,change_config,[extra_db_nodes,[NodeToAdd]],5000) of
	{ok,[NodeToAdd]}->
	    {atomic,_}=rpc:call(IntialNode,mnesia,change_table_copy_type,[schema,NodeToAdd,ram_copies]),
	    Tables=rpc:call(IntialNode,mnesia,system_info,[tables]),	  
	    [{atomic,_}=rpc:call(IntialNode,mnesia,add_table_copy,[Table,NodeToAdd,ram_copies])||Table<-Tables,
											Table/=schema],
	    rpc:call(IntialNode,mnesia,wait_for_tables,[Tables],20*1000),
	    ok;
	Reason ->
	    io:format("NodeToAdd,IntialNode,Reason ~p~n",[{NodeToAdd,IntialNode,Reason,?FUNCTION_NAME,?MODULE,?LINE}]),
	    dynamic_install(T,IntialNode) 
    end.
  %  dynamic_install(T,IntialNode).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_textfile(TableTextFiles)->
    %% Missing tables 
    PresentTables=[Table||Table<-mnesia:system_info(tables),
			  true=:=lists:keymember(Table,1,TableTextFiles),
			  Table/=schema],
 %   io:format("PresentTables  ~p~n",[{PresentTables,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    
    LoadInfoRes=[{mnesia:load_textfile(TextFile),Table,TextFile}||{Table,_StorageType,TextFile}<-TableTextFiles,
						      false=:=lists:member(Table,PresentTables)],
 %   io:format("LoadInfo ~p~n",[{LoadInfo,node(),?FUNCTION_NAME,?MODULE,?LINE}]),
    AddTableRes=[{N,Table,rpc:call(N,dbase,dynamic_add_table,[Table,StorageType],5000)}||N<-lists:delete(node(),sd:get(dbase_infra)),
											{Table,StorageType,_TextFile}<-TableTextFiles],
    {AddTableRes,LoadInfoRes}.


