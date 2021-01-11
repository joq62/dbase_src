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
    {ok,DbaseList}=setup(),
  %  ?assertEqual(  {ok,[Dbase0,Dbase1,Dbase2]},setup()),
    ?debugMsg("stop setup"),

    ?debugMsg("Start single"),
    ?assertEqual(ok,single(DbaseList)),
    ?debugMsg("stop single"),
    
    ?debugMsg("Start two"),
    ?assertEqual(ok,two(DbaseList)),
    ?debugMsg("stop two"),
    
   
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
two([Dbase0,Dbase1,Dbase2])->
    AllDbaseEnvs=application:get_all_env(dbase),

    % All slavesw have acces to the code in this test , not the case in real system
    ?assertMatch([ok,ok,ok,ok,ok,ok,ok],[rpc:call(Dbase1,application,set_env,[dbase,Par,Val])||{Par,Val}<-AllDbaseEnvs]),
    sys_log:msg(misc_oam:masters(),log,
		["Dbase0 =", Dbase1],
		?MODULE,?LINE),
    ?assertMatch(ok,rpc:call(Dbase1,application,start,[dbase],5*5000)),

     %% Test 
    AppSpecs=rpc:call(Dbase1,db_app_spec,read_all,[],2000),
    AppSpecsId=[AppId||{AppId,_AppVsn,_Type,_Directives,_AppEnvs,_Services}<-AppSpecs],

    ServiceSpecs=rpc:call(Dbase1,db_service_def,read_all,[],2000),  
    ServicesSpecsId=[SpecId||{SpecId,_ServiceId,_ServiceVsn,_StartCmd,_GitPath}<-ServiceSpecs],
    PassWd=rpc:call(Dbase1,db_passwd,read_all,[],2000),
    Servers=rpc:call(Dbase1,db_server,read_all,[],2000),
      
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
%% --------------------------------------------------------------------
single([Dbase0,Dbase1,Dbase2])->
    AllDbaseEnvs=application:get_all_env(dbase),

    % All slavesw have acces to the code in this test , not the case in real system
    ?assertMatch([ok,ok,ok,ok,ok,ok,ok],[rpc:call(Dbase0,application,set_env,[dbase,Par,Val])||{Par,Val}<-AllDbaseEnvs]),
    sys_log:msg(misc_oam:masters(),log,
		["Dbase0 =", Dbase0],
		?MODULE,?LINE),
    ?assertMatch({ok,dbase0@c2,dbase,_},rpc:call(Dbase0,dbase,glurk,[],5000)),
    ?assertMatch(ok,rpc:call(Dbase0,application,start,[dbase],5*5000)),

     %% Test 
    AppSpecs=rpc:call(Dbase0,db_app_spec,read_all,[],2000),
    AppSpecsId=[AppId||{AppId,_AppVsn,_Type,_Directives,_AppEnvs,_Services}<-AppSpecs],

    ServiceSpecs=rpc:call(Dbase0,db_service_def,read_all,[],2000),  
    ServicesSpecsId=[SpecId||{SpecId,_ServiceId,_ServiceVsn,_StartCmd,_GitPath}<-ServiceSpecs],
    PassWd=rpc:call(Dbase0,db_passwd,read_all,[],2000),
    Servers=rpc:call(Dbase0,db_server,read_all,[],2000),
      
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
%% --------------------------------------------------------------------
setup()->
   % start 3 nodes dbase0,1,2 as slaves
    {ok,HostId}=net:gethostname(),
    {ok,Dbase0}=slave:start(HostId,dbase0,"-pa db0 -setcookie abc"),
    {ok,Dbase1}=slave:start(HostId,dbase1,"-pa db1 -setcookie abc"),
    {ok,Dbase2}=slave:start(HostId,dbase2,"-pa db2 -setcookie abc"),

   
    Date=date(),
    ?assertMatch(Date,rpc:call(Dbase0,erlang,date,[])),
    ?assertMatch(Date,rpc:call(Dbase1,erlang,date,[])),
    ?assertMatch(Date,rpc:call(Dbase2,erlang,date,[])),

    ?assertMatch({ok,dbase0@c2,dbase,_},rpc:call(Dbase0,dbase,glurk,[])),
    ?assertMatch({ok,dbase1@c2,dbase,_},rpc:call(Dbase1,dbase,glurk,[])),
    ?assertMatch({ok,dbase2@c2,dbase,_},rpc:call(Dbase2,dbase,glurk,[])),

    {ok,[Dbase0,Dbase1,Dbase2]}.

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
