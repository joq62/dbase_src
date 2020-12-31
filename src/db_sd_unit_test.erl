%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_sd_unit_test).   
   
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

    ?debugMsg("Start sd_test_1"),
    ?assertEqual(ok,sd_test_1()),
    ?debugMsg("stop sd_test_1"),
    
   
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
    ServiceId="s1",
    ServiceVsn="1.0.0",
    AppId1="app1",
    AppVsn1="1.0.0",
    HostId1="host1",
    VmId1="vm1",
    VmDir1="vmdir1",
    Vm1='vm1@host1',
    
    ServiceId="s1",
    ServiceVsn="1.0.0",
    AppId2="app2",
    AppVsn2="1.0.0",
    HostId1="host1",
    VmId2="vm2",
    VmDir2="vmdir2",
    Vm2='vm2@host1',



    ?assertEqual({atomic,ok},db_sd:create({db_sd,ServiceId,ServiceVsn,AppId1,AppVsn1,HostId1,VmId1,VmDir1,Vm1})),
    ?assertMatch([{"s1","1.0.0","app1","1.0.0","host1","vm1","vmdir1",'vm1@host1',_}],db_sd:read_all_info()),

    ?assertEqual({atomic,ok},db_sd:create({db_sd,ServiceId,ServiceVsn,AppId2,AppVsn2,HostId1,VmId2,VmDir2,Vm2})),
    [{"s1","1.0.0","app1","1.0.0","host1","vm1","vmdir1",'vm1@host1',T1},
     {"s1","1.0.0","app2","1.0.0","host1","vm2","vmdir2",'vm2@host1',T2}]=db_sd:read_all_info(),
    ?assertEqual(true,2>erlang:system_time(seconds)-T1),
    ?assertEqual(true,2>erlang:system_time(seconds)-T2),

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
    init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
sd_test_1()->
    %% db_lock timeout set to
    Now=erlang:system_time(seconds),
    [{"s1","1.0.0","app1","1.0.0","host1","vm1","vmdir1",'vm1@host1',T1},
     {"s1","1.0.0","app2","1.0.0","host1","vm2","vmdir2",'vm2@host1',T2}]=db_sd:read_all_info(),
    ?assertEqual(true,2>Now-T1),
    ?assertEqual(true,2>Now-T2),
 
    timer:sleep(3*1000),
    ?assertMatch({aborted,_},db_sd:heartbeat(glurk,'vm1@host1')),
    ?assertEqual({atomic,ok},db_sd:heartbeat(s1,'vm1@host1')),

    [{"s1","1.0.0",_,"1.0.0","host1",_,_,_,T11},
     {"s1","1.0.0",_,"1.0.0","host1",_,_,_,T21}]=db_sd:read_all_info(),

  %  io:format("Now ~p~n", [Now]),
  %  io:format("T1 ~p~n", [T1]),
  %  io:format("T11 ~p~n", [T11]),
  %  io:format("T2 ~p~n", [T2]),
  %  io:format("T21 ~p~n", [T21]),

    ?assertEqual(true,2>Now-T11),
    ?assertEqual(false,2>T21-Now),
    ok.
