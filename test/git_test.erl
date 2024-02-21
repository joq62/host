%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(git_test).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("host.hrl").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_repo(),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(1000),
    init:stop(),
    ok.


    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_repo()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,{error,["RepoDir doesnt exists, need to clone"]}}=host:is_repo_updated(),
    ok=host:clone_repo(),
    true=host:is_repo_updated(),
    {error,{error,["Already updated ","catalog/host_specs"]}}=host:update_repo(),
    
    ["c200","c201","c202","c230","c50"]=lists:sort(host:get_hostnames()),
   
    {ok,Map}=host:get_host_map("c50"),
    [
     {application_config,[]},
     {hostname,"c50"},
     {ip,"172.18.221.251"},
     {password,_},
     {ssh_port,22},
     {userid,"joq62"}]=lists:sort(maps:to_list(Map)),

    {ok,"c50"}=host:get_info(hostname,"c50"),
    {ok,"172.18.221.251"}=host:get_info(ip,"c50"),
    {ok,22}=host:get_info(ssh_port,"c50"),
    {ok,[]}=host:get_info(application_config,"c50"),
    {ok,"joq62"}=host:get_info(userid,"c50"),
    {ok,"festum01"}=host:get_info(password,"c50"),

    {error,{badkey,glurk},_,_,_}=host:get_info(glurk,"c50"),
    {error,["Host doens't exists",glurk]}=host:get_info(password,glurk),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    true=filelib:is_dir(?MainDir),
    file:del_dir_r(?MainDir),
    false=filelib:is_dir(?MainDir),
    file:make_dir(?MainDir),
    true=filelib:is_dir(?MainDir),
    false=filelib:is_dir(?RepoDir),

    
    ok.
