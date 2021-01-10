%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_cluster_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("Start setup"),
    ?assertEqual(ok,setup()),
    ?debugMsg("stop setup"),

%    ?debugMsg("Start lock_test_1"),
%    ?assertEqual(ok,lock_test_1()),
%    ?debugMsg("stop lock_test_1"),
    
   
      %% End application tests
    ?debugMsg("Start cleanup"),
    ?assertEqual(ok,cleanup()),
    ?debugMsg("Stop cleanup"),

    ?debugMsg("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
   % start 3 nodes dbase0,1,2 as slaves
    {ok,HostId}=net:gethostname(),
    Args="-setcookie abc", 
    {ok,Dbase0}=slave:start(HostId,dbase0,Args),
    {ok,Dbase1}=slave:start(HostId,dbase1,Args),
    {ok,Dbase2}=slave:start(HostId,dbase2,Args),

    Date=date(),
    ?assertMatch(Date,rpc:call(Dbase0,erlang,date,[])),
    ?assertMatch(Date,rpc:call(Dbase1,erlang,date,[])),
    ?assertMatch(Date,rpc:call(Dbase2,erlang,date,[])),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
  %  init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
