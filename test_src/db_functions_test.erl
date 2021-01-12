%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_functions_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]).

-include("src/db_app_spec.hrl").

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

    ?debugMsg("Start db_app_spec"),
    ?assertEqual(ok,db_app_spec()),
    ?debugMsg("stop db_app_spec"),
    
   
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

db_app_spec()->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(), 
    mnesia:create_table(app_spec,[{attributes, record_info(fields,app_spec)}]),
    
    {ok,[DbAppSpecInfo]}=file:consult("../../app_specs/a.org"),
    io:format("~p~n",[DbAppSpecInfo]), 
    %% create via file
    ?assertMatch({atomic,ok},db_app_spec:create(DbAppSpecInfo)),
    ?assertMatch([{"dbase_100_c0.app_spec",
		   "1.0.0",
		   worker,
		   "c0",
		   "dbase",
		   "dbase_100",
		   "abc",
		   [[{service_id,"dbase"},
		     {service_vsn,"1.0.0"},
		     {git_path,"https://github.com/joq62/dbase.git"},
		     {start_cmd,{application,start,[dbase]}},
		     {env_vars,
		      [{dbase,git_user,"joq62"},
		       {dbase,git_pw,"20Qazxsw20"},
		       {dbase,cl_dir,"cluster_config"},
		       {dbase,cl_file,"cluster_info.hrl"},
		       {dbase,app_specs_dir,"app_specs"},
		       {dbase,service_specs_dir,"service_specs"},
		       {dbase,dbase_nodes,[dbase@c0,dbase@c1,dbase@c2]}]}]]}],
		 db_app_spec:read_all()),

    ?assertMatch([{"dbase_100_c0.app_spec","1.0.0",_,_,_,
		   _,_,_}],db_app_spec:read("dbase_100_c0.app_spec")),
    [ServiceInfo]=db_app_spec:services("dbase_100_c0.app_spec"),
    ?assertMatch([[{service_id,"dbase"},
		   {service_vsn,"1.0.0"},
		   {git_path,"https://github.com/joq62/dbase.git"},
		   {start_cmd,{application,start,[dbase]}},
		   {env_vars,
		    [{dbase,git_user,"joq62"},
		     {dbase,git_pw,"20Qazxsw20"},
		     {dbase,cl_dir,"cluster_config"},
		       {dbase,cl_file,"cluster_info.hrl"},
		     {dbase,app_specs_dir,"app_specs"},
		     {dbase,service_specs_dir,"service_specs"},
		     {dbase,dbase_nodes,[dbase@c0,dbase@c1,dbase@c2]}]}]],
		 ServiceInfo),

    ?assertMatch("c0",db_app_spec:host("dbase_100_c0.app_spec")),
    ?assertMatch("dbase",db_app_spec:vm_id("dbase_100_c0.app_spec")),
    ?assertMatch("dbase_100",db_app_spec:vm_dir("dbase_100_c0.app_spec")),
    ?assertMatch("abc",db_app_spec:cookie("dbase_100_c0.app_spec")),

    ?assertMatch("https://github.com/joq62/dbase.git",db_app_spec:git_path("dbase_100_c0.app_spec")),
    ?assertMatch({application,start,[dbase]},db_app_spec:start_cmd("dbase_100_c0.app_spec")),
    ?assertMatch([{dbase,git_user,"joq62"},
		  {dbase,git_pw,"20Qazxsw20"},
		  {dbase,cl_dir,"cluster_config"},
		  {dbase,cl_file,"cluster_info.hrl"},
		  {dbase,app_specs_dir,"app_specs"},
		  {dbase,service_specs_dir,"service_specs"},
		  {dbase,dbase_nodes,[dbase@c0,dbase@c1,dbase@c2]}],
		 db_app_spec:app_env_vars("dbase_100_c0.app_spec")),


 
    
    
    
    
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

    ?assertEqual({atomic,[ok]},db_sd:remove_orphanes(2)),
    ?assertMatch([{"s1","1.0.0","app1","1.0.0","host1","vm1","vmdir1",'vm1@host1',_}],
		 db_sd:read_all_info()),
    ok.
