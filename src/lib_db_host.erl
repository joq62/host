%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_db_host).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("db_host.hrl").

%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/1,delete/1]).
-export([get_info/1,get_all/0,get/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([git_clone_load/0]).

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {type,set}
				]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
create(HostName)->
    Record=#?RECORD{
		    hostname=HostName,
		    ip=undefined,
		    ssh_port=undefined,
		    user=undefined,
		    passwd=undefined,
		    application_config=undefined
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

create(HostName,Ip,SshPort,User,Passwd,ApplConfig)->
    Record=#?RECORD{
		    hostname=HostName,
		    ip=Ip,
		    ssh_port=SshPort,
		    user=User,
		    passwd=Passwd,
		    application_config=ApplConfig
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

delete(HostName) ->
    F = fun() -> 
		mnesia:delete({?TABLE,HostName})
		    
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

member(HostName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.hostname==HostName])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

get_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.hostname,R#?RECORD.ip,R#?RECORD.ssh_port,
      R#?RECORD.user,R#?RECORD.passwd,R#?RECORD.application_config}||R<-Z].

get_info(HostName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.hostname==HostName])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.hostname,R#?RECORD.ip,
			    R#?RECORD.ssh_port,R#?RECORD.user,R#?RECORD.passwd,
			    R#?RECORD.application_config}||R<-Z],
		   Info
	   end,
    Result.

get(Key,HostName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.hostname==HostName])),
    Result=case Z of
	       []->
		   {error,[eexist,HostName,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       hostname->
			   {ok,R#?RECORD.hostname};
		       ip->
			   {ok,R#?RECORD.ip};
		       port->
			   {ok,R#?RECORD.ssh_port};
		       user->
			   {ok,R#?RECORD.user};
		       passwd->
			   {ok,R#?RECORD.passwd};
		       application_config->
			   {ok,R#?RECORD.application_config};
		       Err ->
			   {error,['Key eexists',Err,HostName,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.hostname||R<-Z].
    

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

git_clone_load()->
    ok=create_table(),
    Result=case git_clone() of
	       {error,Reason}->
		   {error,Reason};
	       {ok,TempDirName,SpecDir}->
		   case from_file(SpecDir) of
		       {error,Reason}->
			   os:cmd("rm -rf "++TempDirName),	
			   {error,Reason};
		       LoadResult->
			   os:cmd("rm -rf "++TempDirName),	
			   LoadResult
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

git_clone()->
    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
    ok=file:make_dir(TempDirName),
    GitDir=filename:join(TempDirName,?HostSpecDir),
    GitPath=?GitPathHostSpecs,
    os:cmd("rm -rf "++GitDir),    
    ok=file:make_dir(GitDir),
    GitResult=cmn_appl:git_clone_to_dir(node(),GitPath,GitDir),
    Result=case filelib:is_dir(GitDir) of
	       false->
		   {error,[failed_to_clone,GitPath,GitResult]};
	       true->
		   {ok,TempDirName,GitDir}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

from_file(ApplSpecDir)->
    {ok,FileNames}=file:list_dir(ApplSpecDir),
    from_file(FileNames,ApplSpecDir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    NewAcc=case filename:extension(FileName) of
	       ?Extension->
		   FullFileName=filename:join(Dir,FileName),
		   case file:consult(FullFileName) of
			{error,Reason}->
			    [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
			{ok,[{host_spec,Info}]}->
			 %  io:format("Info ~p~n",[{Info,?MODULE,?LINE}]),
			    {hostname,HostName}=lists:keyfind(hostname,1,Info),
			    {ip,Ip}=lists:keyfind(ip,1,Info),
			    {ssh_port,SshPort}=lists:keyfind(ssh_port,1,Info),
			    {user,User}=lists:keyfind(user,1,Info),
			    {passwd,Passwd}=lists:keyfind(passwd,1,Info),
			    {application_config,ApplConfig}=lists:keyfind(application_config,1,Info),
			    case create(HostName,Ip,SshPort,User,Passwd,ApplConfig) of
				{atomic,ok}->
				    [{ok,FileName}|Acc];
				{error,Reason}->
				    [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
			    end
		   end;
	       _ -> 
		   Acc
		   %[{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
			   
