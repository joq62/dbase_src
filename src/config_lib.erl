%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(config_lib).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Definition

%% --------------------------------------------------------------------


%-compile(export_all).
-export([init_dbase/0,
	 load_app_specs/3,
	 read_app_specs/0,
	 read_app_spec/1,
	 load_service_specs/3,
	 read_service_specs/0,
	 read_service_spec/1,
	 load_cluster_config/4
	]).
	 
	
%% ====================================================================
%% External functions
%% ====================================================================
init_dbase()->
    
    {ok,GitUser}=application:get_env(git_user),
    {ok,GitPassWd}=application:get_env(git_pw),
    {ok,ClusterConfigDir}=application:get_env(cl_dir),
    {ok,ClusterConfigFileName}=application:get_env(cl_file),
    {ok,AppSpecsDir}=application:get_env(app_specs_dir),
    {ok,_ServiceSpecsDir}=application:get_env(service_specs_dir),
  
 
    ok=load_app_specs(AppSpecsDir,GitUser,GitPassWd),
    
  %  ok=load_service_specs(ServiceSpecsDir,GitUser,GitPassWd),
 
    ok=load_cluster_config(ClusterConfigDir,ClusterConfigFileName,GitUser,GitPassWd),
 %   io:format("Line = ~p~n",[{?MODULE,?LINE}]),
    ok.

%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

read_app_spec(AppId)->
    if_db:call(db_app_spec,read,[AppId]).
%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

read_app_specs()->
    if_db:call(db_app_spec,read_all,[]).
    
%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

load_app_specs(AppSpecDir,GitUser,GitPassWd)->
     %% Get initial configuration
    os:cmd("rm -rf "++AppSpecDir),
    GitCmd="git clone https://"++GitUser++":"++GitPassWd++"@github.com/"++GitUser++"/"++AppSpecDir++".git",
    os:cmd(GitCmd),
    Result=case file:list_dir(AppSpecDir) of
	       {ok,FileNames}->
		   SpecFileNames=[filename:join(AppSpecDir,FileName)||FileName<-FileNames,
					       ".app_spec"==filename:extension(FileName)],
		%   io:format("SpecFileNames= ~p~n",[{SpecFileNames,?MODULE,?LINE}]),
		   L1=[file:consult(FileName)||FileName<-SpecFileNames],
		%   io:format("Info= ~p~n",[{L1,?MODULE,?LINE}]),
		   L2=[Info||{ok,[Info]}<-L1],
	%	   io:format("Info= ~p~n",[{L2,?MODULE,?LINE}]),
		   
		  % DbaseResult=[R||R<-dbase:init_table_info(L2),
		%		   R/={atomic,ok}],
		   DbaseResult=[R||R<-dbase_lib:create_table(L2),
				   R/={atomic,ok}],
		   case DbaseResult of
			[]->
			   ok;
		       Reason->
			   {error,Reason}
		   end;
	       {error,Reason} ->
		   {error,Reason}
	   end, 
    Result.
    

%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

read_service_spec(Id)->
    if_db:call(db_service_def,read,[Id]).
%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

read_service_specs()->
    if_db:call(db_service_def,read_all,[]).
    
%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------

load_service_specs(SpecDir,GitUser,GitPassWd)->
     %% Get initial configuration
    os:cmd("rm -rf "++SpecDir),
    GitCmd="git clone https://"++GitUser++":"++GitPassWd++"@github.com/"++GitUser++"/"++SpecDir++".git",
    os:cmd(GitCmd),
    Result=case file:list_dir(SpecDir) of
	       {ok,FileNames}->
		   SpecFileNames=[filename:join(SpecDir,FileName)||FileName<-FileNames,
								   ".service_spec"==filename:extension(FileName)],
		   L1=[file:consult(FileName)||FileName<-SpecFileNames],
		   L2=[Info||{ok,[Info]}<-L1],
		   L3=dbase_lib:create_table(L2),
		   DbaseResult=[R||R<-L3,
				   R/={atomic,ok}],
		   
		   case DbaseResult of
			[]->
			   ok;
		       Reason->
			   {error,Reason}
		   end;
	       {error,Reason} ->
		   {error,Reason}
	   end, 
    Result.
    
%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%% --------------------------------------------------------------------
load_cluster_config(ClusterConfigDir,ClusterConfigFileName,GitUser,GitPassWd)->

%% Get initial configuration
    os:cmd("rm -rf "++ClusterConfigDir),
    GitCmd="git clone https://"++GitUser++":"++GitPassWd++"@github.com/"++GitUser++"/"++ClusterConfigDir++".git",
    os:cmd(GitCmd),
    ConfigFilePath=filename:join([".",ClusterConfigDir,ClusterConfigFileName]),
    {ok,Info}=file:consult(ConfigFilePath),
    dbase_lib:create_table(Info),
    ok.
    
    
