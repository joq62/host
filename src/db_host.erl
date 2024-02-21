%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(db_host).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api"). 
 

%% API

-export([
	 create/1,
	 all_hosts/0,
	 get_info/1,
	 
	 get_ip/1,
	 set_ip/2,
	 get_port/1,
	 set_port/2,

	 get_user/1,
	 set_user/2,
	 get_passwd/1,
	 set_passwd/2,

	 get_appl_config/1,
	 set_appl_config/2,

	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% get all hostnames that are stored in dbase
%% @end
%%--------------------------------------------------------------------
-spec all_hosts() -> ListOfHostnames :: term().

all_hosts()->
    gen_server:call(?SERVER, {all_hosts},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_info(HostName :: string()) -> {ok,HostInfo :: term()} | {error, Error :: term()}.

get_info(HostName)->
    gen_server:call(?SERVER, {get_info,HostName},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new instance with undefined data 
%% @end
%%--------------------------------------------------------------------
-spec create(HostName :: string()) -> ok | {error, Error :: term()}.

create(HostName)->
    gen_server:call(?SERVER, {create,HostName},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% set ip adress Ip to  host HostName
%% @end
%%--------------------------------------------------------------------
-spec set_ip(Ip :: string(), HostName :: string()) -> ok | {error, Error :: term()}.

set_ip(Ip,HostName)->
    gen_server:call(?SERVER, {set_ip,Ip,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_ip(HostName :: string()) -> {ok,Ip :: string()} | {error, Error :: term()}.

get_ip(HostName)->
    gen_server:call(?SERVER, {get_ip,HostName},infinity).

%%--------------------------------------------------------------------
%% @doc
%% set port Port to  host HostName
%% @end
%%--------------------------------------------------------------------
-spec set_port(Port :: integer(), HostName :: string()) -> ok | {error, Error :: term()}.

set_port(Port,HostName)->
    gen_server:call(?SERVER, {set_port,Port,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get port from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_port(HostName :: string()) -> {ok,Port :: integer()} | {error, Error :: term()}.

get_port(HostName)->
    gen_server:call(?SERVER, {get_port,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% set user User to  host HostName
%% @end
%%--------------------------------------------------------------------
-spec set_user(User :: string(), HostName :: string()) -> ok | {error, Error :: term()}.

set_user(User,HostName)->
    gen_server:call(?SERVER, {set_user,User,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get user from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_user(HostName :: string()) -> {ok,User :: string()} | {error, Error :: term()}.

get_user(HostName)->
    gen_server:call(?SERVER, {get_user,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% set passwd PassWd  to  host HostName
%% @end
%%--------------------------------------------------------------------
-spec set_passwd(PassWd :: string(), HostName :: string()) -> ok | {error, Error :: term()}.

set_passwd(PassWd,HostName)->
    gen_server:call(?SERVER, {set_passwd,PassWd,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get password from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_passwd(HostName :: string()) -> {ok,PassWd :: string()} | {error, Error :: term()}.

get_passwd(HostName)->
    gen_server:call(?SERVER, {get_passwd,HostName},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% set application config ApplConfig for host HostName
%% @end
%%--------------------------------------------------------------------
-spec set_appl_config(ApplConfig :: term(), HostName :: string()) -> ok | {error, Error :: term()}.

set_appl_config(ApplConfig ,HostName)->
    gen_server:call(?SERVER, {set_appl_config,ApplConfig ,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get application configuration for host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_appl_config(HostName :: string()) -> {ok,ApplConfig :: term()} | {error, Error :: term()}.

get_appl_config(HostName)->
    gen_server:call(?SERVER, {get_appl_config,HostName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
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


stop()-> gen_server:call(?SERVER, {stop},infinity).

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
    case lists:delete({etcd,node()},rd:fetch_resources(etcd)) of
	[]->
	    ok=lib_etcd_host:create_table(),    
	    ClusterSpecList=lib_etcd_host:git_clone_load(),
	    Ok_ClusterSpec=[X||{ok,X}<-ClusterSpecList],
	    FailedToCreate=[X||{error,X}<-ClusterSpecList],
	    ?LOG_NOTICE("Successfully created  ",[Ok_ClusterSpec]),
	    case FailedToCreate of
		[]->
		    ok;
		_->
		    ?LOG_NOTICE("Failed to create   ",[FailedToCreate])
	    end;
	_ ->
	    ok
    end,
    
 
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
handle_call({all_hosts}, _From, State) ->
    Reply=lib_etcd_host:get_all_id(),
    {reply, Reply, State};


handle_call({get_info,HostName}, _From, State) ->
    Reply=lib_etcd_host:get_info(HostName),
    {reply, Reply, State};

handle_call({get_ip,HostName}, _From, State) ->
    Reply=lib_etcd_host:get(ip,HostName),
    {reply, Reply, State};

handle_call({get_port,HostName}, _From, State) ->
    Reply=lib_etcd_host:get(port,HostName),
    {reply, Reply, State};

handle_call({get_user,HostName}, _From, State) ->
    Reply=lib_etcd_host:get(user,HostName),
    {reply, Reply, State};

handle_call({get_passwd,HostName}, _From, State) ->
    Reply=lib_etcd_host:get(passwd,HostName),
    {reply, Reply, State};

handle_call({get_appl_config,HostName}, _From, State) ->
    Reply=lib_etcd_host:get(application_config,HostName),
    {reply, Reply, State};



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
