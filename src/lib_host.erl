%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_host).
  
-include("host.hrl").
-define(UpToDate,"Up to date").
-define(NotUpToDate,"Not up to date").
 
%% API
-export([
	 check_update_repo_return_host_maps/2,
	 get_paths/1,
	 is_appl_updated/1,
	 update_appl/1,
	 clone_appl/3
	]).

-export([
	 is_repo_updated/1,
	 update_repo/1,
	 clone_repo/2
	]).

%%%===================================================================
%%% API
%%%===================================================================





%%********************* Host *****************************************    

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_paths(ApplDir)->
    Ebin=filename:join([ApplDir,"ebin"]),
    Priv=filename:join([ApplDir,"priv"]),
    true=filelib:is_dir(Ebin),
    Paths=case filelib:is_dir(Priv) of
	      true->
		  [Ebin,Priv];
	      false->
		  [Ebin]
	  end,
    {ok,Paths}.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_appl_updated(AppDir)->
    Result=case filelib:is_dir(AppDir) of
	       false->
		   {error,["AppDir doesnt exists, need to clone"]};
	       true->
		   {ok,is_up_to_date(AppDir)}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_appl(AppDir)->
    true=filelib:is_dir(AppDir),
    Result=merge(AppDir),   
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
clone_appl(ApplId,ApplDir,InventoryFile)->
    {ok,Info}=file:consult(InventoryFile),
    [GitPath]=[maps:get(git_path,Map)||Map<-Info,
				       ApplId=:=maps:get(id,Map)],
    file:del_dir_r(ApplDir),
    ok=file:make_dir(ApplDir),
    ok=clone(ApplDir,GitPath),   
    ok.


%%********************* Repo ************************************


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
check_update_repo_return_host_maps(RepoDir,RepoGit)->
    case is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=clone_repo(RepoDir,RepoGit);
	{ok,false} ->
	    ok=update_repo(RepoDir);
	{ok,true}->
	    ok
    end,
    
    {ok,AllFileNames}=file:list_dir(RepoDir),
    AllFullFilenames=[filename:join([RepoDir,FileName])||FileName<-AllFileNames],
    HostFiles=[FullFileName||FullFileName<-AllFullFilenames,
			     ?Extension==filename:extension(FullFileName)],
    FileConsult=[file:consult(HostFile)||HostFile<-HostFiles],
    HostSpecMaps=[Map||{ok,[Map]}<-FileConsult],
    {ok,HostSpecMaps}. 
	       
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_repo_updated(RepoDir)->
    Result=case filelib:is_dir(RepoDir) of
	       false->
		   {error,["RepoDir doesnt exists, need to clone"]};
	       true->
		   {ok,is_up_to_date(RepoDir)}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_repo(RepoDir)->
    true=filelib:is_dir(RepoDir),
    Result=merge(RepoDir),   
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
clone_repo(RepoDir,RepoGit)->
    file:del_dir_r(RepoDir),
    ok=file:make_dir(RepoDir),
    Result=clone(RepoDir,RepoGit),   
    Result.



%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

get_tags(LocalRepo)->
    TagString=os:cmd("git -C "++LocalRepo++" "++"tag"),
    T1=[S||S<-string:split(TagString, "\n", all),
	     []=/=S],
    Tags=lists:reverse(lists:sort(T1)),
    Tags.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
merge(LocalRepo)->
    Result=case is_up_to_date(LocalRepo) of
	       false->
		   os:cmd("git -C "++LocalRepo++" "++"merge  "),
		   ok;
	       true->
		   {error,["Already updated ",LocalRepo]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

clone(RepoDir,RepoGit)->
    []=os:cmd("git clone -q "++RepoGit++" "++RepoDir),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_up_to_date(LocalRepo)->

    _Fetch=os:cmd("git -C "++LocalRepo++" "++"fetch origin "),
    Status=os:cmd("git -C "++LocalRepo++" status -uno | grep -q 'Your branch is up to date'  && echo Up to date || echo Not up to date"),
    [FilteredGitStatus]=[S||S<-string:split(Status, "\n", all),
			  []=/=S],
    Result=case FilteredGitStatus of
	       ?UpToDate->
		   true;
	       ?NotUpToDate->
		   false
	   end,
    Result.
