%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_init_test).   
   
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

    ?debugMsg("Start single_node"),
    ?assertEqual(ok,single_node()),
    ?debugMsg("stop single_node"),
    
    ?debugMsg("Start lock_test_1"),
    ?assertEqual(ok,lock_test_1()),
    ?debugMsg("stop lock_test_1"),
    
   
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
    ok.

single_node()->
    AppSpecs=db_app_spec:read_all(),
    AppSpecsId=[AppId||{AppId,_AppVsn,_Type,_Directives,_AppEnvs,_Services}<-AppSpecs],

    ServiceSpecs=db_service_def:read_all(),  
    ServicesSpecsId=[SpecId||{SpecId,_ServiceId,_ServiceVsn,_StartCmd,_GitPath}<-ServiceSpecs],
    PassWd=db_passwd:read_all(),
    Servers=db_server:read_all(),
      
    ?assertMatch(["master_100_c1.app_spec",
		  "dbase_100_c2.app_spec",
		  "calc_c_100.app_spec",
		  "master_100_c0.app_spec",
		  "dbase_100_c0.app_spec",
		  "dbase_100_c1.app_spec",
		  "calc_a_100.app_spec",
		  "calc_b_100.app_spec",
		  "master_100_c2.app_spec"],
		 AppSpecsId),

  ?assertMatch(["multi_100.service_spec",
		"server_100.service_spec",
		"adder_100.service_spec",
		"divi_100.service_spec",
		"common_100.service_spec",
		"master_100.service_spec",
		"calc_100.service_spec",
		"dbase_100.service_spec"],
	       ServicesSpecsId),

    ?assertMatch([{"joq62","20Qazxsw20"}],
		 PassWd),
    ?assertMatch([{"c2","joq62","festum01","192.168.0.202",22,not_available},
		  {"c1","joq62","festum01","192.168.0.201",22,not_available},
		  {"c0","joq62","festum01","192.168.0.200",22,not_available}],
		 Servers),
    
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
lock_test_1()->
    %% db_lock timeout set to
%
    LockId=test_lock,
    LockTimeOut=3,   %% 3 Seconds
    ?assertMatch({atomic,ok},db_lock:create(LockId,LockTimeOut)),
    
     
    ?assertEqual(true,db_lock:is_open(LockId,LockTimeOut)),
    timer:sleep(1*1000),
    ?assertEqual(false,db_lock:is_open(LockId,LockTimeOut)),
    timer:sleep(3*1000),
    ?assertEqual(true,db_lock:is_open(LockId,LockTimeOut)),

    ?assertMatch({atomic,ok},db_lock:delete(LockId)),
    ok.
