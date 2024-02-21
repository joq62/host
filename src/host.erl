%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(host). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("host.hrl").



%% API

-export([
	
	 is_repo_updated/0,
	 update_repo/0,
	 clone_repo/0
	]).

-export([
	 get_info/2,
	 get_hostnames/0,
	 get_host_maps/0,
	 get_host_map/1,
	 is_appl_updated/1,
	 update_appl/1,
	 clone_appl/1
	 
	]).

%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		
		main_dir,
	        repo_dir,
		host_spec_maps,
	        repo_git
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%********************* Appl *****************************************

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_info(Key:: atom(),HostName :: string()) -> 
	  {ok,Value:: term()} | {error, Reason :: term()}.
get_info(Key,HostName) ->
    gen_server:call(?SERVER,{get_info,Key,HostName},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_hostnames() -> 
	  {ok,HostNamesList :: term()} | {error, Reason :: term()}.
get_hostnames() ->
    gen_server:call(?SERVER,{get_hostnames},infinity).
%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_host_maps() -> 
	  {ok,HostMapsList :: term()} | {error, Reason :: term()}.
get_host_maps() ->
    gen_server:call(?SERVER,{get_host_maps},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_host_map(Host :: string()) -> 
	  {ok,HostMap :: map()} | {error, Reason :: term()}.
get_host_map(Host) ->
    gen_server:call(?SERVER,{get_host_map,Host},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_appl_updated(ApplId :: string()) -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_appl_updated(ApplId) ->
    gen_server:call(?SERVER,{is_appl_updated,ApplId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_appl(ApplId :: string()) -> 
	  ok | {error, Reason :: term()}.
update_appl(ApplId) ->
    gen_server:call(?SERVER,{update_appl,ApplId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_appl(ApplId :: string()) -> 
	  ok | {error, Reason :: term()}.
clone_appl(ApplId) ->
    gen_server:call(?SERVER,{clone_appl,ApplId},infinity).





%%********************* Repo ************************************

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_repo_updated() -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_repo_updated() ->
    gen_server:call(?SERVER,{is_repo_updated},infinity).

%%--------------------------------------------------------------------
%% @doc
%% repo   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_repo() -> 
	  ok | {error, Reason :: term()}.
update_repo() ->
    gen_server:call(?SERVER,{update_repo},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_repo() -> 
	  ok | {error, Reason :: term()}.
clone_repo() ->
    gen_server:call(?SERVER,{clone_repo},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Return all repos   
%% 
%% @end
%%--------------------------------------------------------------------
-spec repos() -> 
	  {ok,ListOfRepos :: term()} | {error, Reason :: term()}.
repos() ->
    gen_server:call(?SERVER,{repos},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Returns the path to "ebin" dir and if needed "priv" dir 
%% related to Application Id  
%% 
%% @end
%%--------------------------------------------------------------------
-spec paths(ApplicationId :: string()) -> 
	  {ok,ListOfPaths :: term()} | {error, Reason :: term()}.
paths(ApplicationId) ->
    gen_server:call(?SERVER,{paths,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Resturn the app for application ApplicationId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec app(ApplicationId::string()) -> 
	  {ok,App::atom()} | {error, Reason :: term()}.
app(ApplicationId) ->
    gen_server:call(?SERVER,{app,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% git merge for the repo LocalRepo 
%% 
%% @end
%%--------------------------------------------------------------------
-spec merge(LocalRepo::string()) -> ok | 
	  {error, Reason :: term()}.
merge(LocalRepo) ->
    gen_server:call(?SERVER,{merge,LocalRepo},infinity).

%%--------------------------------------------------------------------
%% @doc
%% stops all kubeletes and deletes their dirs 
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete_cluster(ClusterId::string()) -> ok | 
	  {error, Error :: term()}.
delete_cluster(ClusterId) ->
    gen_server:call(?SERVER,{delete_cluster,ClusterId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill()->
    gen_server:call(?SERVER, {kill},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    
    
%    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    main_dir=?MainDir,
	    repo_dir=?RepoDir,
	    host_spec_maps=[],
	    repo_git=?RepoGit
	  
	    
	   },0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

%%********************* Appl *****************************************
handle_call({get_host_maps}, _From, State) ->
    Reply=State#state.host_spec_maps,
    {reply, Reply, State};

handle_call({get_hostnames}, _From, State) ->
    Reply= [maps:get(hostname,Map)||Map<-State#state.host_spec_maps],
    {reply, Reply, State};

handle_call({get_host_map,HostName}, _From, State) ->
    Reply=case [Map||Map<-State#state.host_spec_maps,
		     HostName==maps:get(hostname,Map)] of
	      []->
		  {error,["Host doens't exists",HostName]};
	      [Map]->
		  {ok,Map}
	  end,
    {reply, Reply, State};

handle_call({get_info,Key,HostName}, _From, State) ->
    HostSpecMaps=State#state.host_spec_maps,
    Result=try lib_host:get_info(Key,HostName,HostSpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Value}->
		  {ok,Value};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

handle_call({update_appl,ApplId}, _From, State) ->
    ApplDir=filename:join([State#state.main_dir,ApplId]),
    Result=try lib_catalog:update_inventory(ApplDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};


%%********************* Repo ************************************
    
handle_call({is_repo_updated}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_host:is_repo_updated(RepoDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		  NewState=State,
		  IsUpdated;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({update_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_host:update_repo(RepoDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({clone_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    RepoGit=State#state.repo_git,
    Result=try lib_host:clone_repo(RepoDir,RepoGit) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		%  io:format("CloneResult ~p~n",[{ok,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};
 

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
    io:format("timeout ~p~n",[{?MODULE,?LINE}]),
    RepoDir=State#state.repo_dir,
    RepoGit=State#state.repo_git,
    Result=try lib_host:check_update_repo_return_host_maps(RepoDir,RepoGit) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    NewState=case Result of
		 {ok,HostSpecMaps}->
		     io:format("HostSpecMaps ~p~n",[{HostSpecMaps,?MODULE,?LINE}]),
		     NewState=State#state{host_spec_maps=HostSpecMaps};
		 ErrorEvent->
		     io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		     NewState=State
	     end,
    
    
    {noreply, NewState};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
