%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("db_server.hrl").
-include("db_deployment.hrl").
-include("db_deployment_spec.hrl").
-include("db_passwd.hrl").
-include("db_sd.hrl").
-include("db_service_def.hrl").
-include("db_vm.hrl").
-include("db_log.hrl").
-include("db_app_spec.hrl").
-include("db_lock.hrl").

%% --------------------------------------------------------------------


%% External exports
-export([start/0,
	 db_init/1,
	 add_node/1,
	 add_extra_nodes/1,
	 create_table/1
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

start()->
    misc_log:msg(log,
		["Line ="],
		 node(),?MODULE,?LINE),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
   
    %% Check if there dbase nodes are running
    {ok,DbaseNodes}=application:get_env(dbase,dbase_nodes),
    PingResult=[rpc:call(DbaseNode,dbase,ping,[])||DbaseNode<-lists:delete(node(),DbaseNodes)],
    misc_log:msg(log,
		["PingResult ",PingResult],
		 node(),?MODULE,?LINE),
   
    Nodes=[Node||{pong,Node,_}<-PingResult],
    misc_log:msg(log,
		 ["Nodes ",Nodes],
		 node(),?MODULE,?LINE),
    
    db_init(lists:delete(node(), Nodes)).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_table([])->
    [];
create_table(TableInfo) ->
    create_table(TableInfo,[]).
create_table([],Result)->
    Result;
create_table([L|T],Acc)->
    [Module|_]=L,
   % Arg=list_to_tuple(L),
    Arg=L,
%    io:format("~p~n",[{?MODULE,?LINE,Module,create,Arg}]),
    R=apply(Module,create,[Arg]),
    %io:format("~p~n",[{?MODULE,?LINE,Module,create,Arg}]),

    NewAcc=[R|Acc],
    create_table(T,NewAcc).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

add_node(Vm)->
    
    rpc:call(Vm,mnesia,stop,[]),
    mnesia:delete_schema([Vm]),
    ok=rpc:call(Vm,mnesia,start,[]),
   
    mnesia:change_config(extra_db_nodes, [Vm]),
    mnesia:add_table_copy(schema, Vm,ram_copies),
	    % Update with tables
    mnesia:add_table_copy(server, Vm, ram_copies),
%    mnesia:add_table_copy(deployment, Vm, ram_copies),
%    mnesia:add_table_copy(deployment_spec, Vm, ram_copies),
    mnesia:add_table_copy(passwd, Vm, ram_copies),
    mnesia:add_table_copy(sd, Vm, ram_copies),
%    mnesia:add_table_copy(service_def, Vm, ram_copies),
%    mnesia:add_table_copy(vm, Vm, ram_copies),
    mnesia:add_table_copy(log, Vm, ram_copies),
    mnesia:add_table_copy(app_spec, Vm, ram_copies),
    mnesia:add_table_copy(lock, Vm, ram_copies),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

db_init([])->
    % Singe Dbase no other running
    mnesia:create_table(server,[{attributes, record_info(fields, server)}]),
 %   mnesia:create_table(deployment,[{attributes, record_info(fields,deployment)}]),
 %   mnesia:create_table(deployment_spec,[{attributes, record_info(fields,deployment_spec)}]),
    mnesia:create_table(passwd,[{attributes, record_info(fields,passwd)}]),
    mnesia:create_table(sd,[{attributes, record_info(fields,sd)},
			    {type,bag}]),
  %  mnesia:create_table(service_def,[{attributes, record_info(fields,service_def)}]),
 %   mnesia:create_table(vm,[{attributes, record_info(fields,vm)}]),    
    mnesia:create_table(log,[{attributes, record_info(fields,log)},
			    {type,bag}]),    
    mnesia:create_table(app_spec,[{attributes, record_info(fields,app_spec)}]),   
    mnesia:create_table(lock,[{attributes, record_info(fields,lock)}]),   

   
    % Initiate from git
 
    config_lib:init_dbase(),

    misc_log:msg(log,
		 ["Initiated mnesia in single mode"],
		 node(),?MODULE,?LINE),
    ok;

db_init(AllNodes)-> 
    % Other dbases are runnung - copy their dbases tables 
    misc_log:msg(log,
		["Initiated mnesia ini existing cluster using nodes",AllNodes],
		node(),?MODULE,?LINE),
    add_extra_nodes(AllNodes).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

add_extra_nodes([])->
    ok;
add_extra_nodes([Node|T])->
    misc_log:msg(log,
		 ["add_extra_nodes",Node,T],
		 node(),?MODULE,?LINE),
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok,[Node]}->
	 %   io:format(" ~p~n",[{?MODULE,?LINE,Node}]),
	    mnesia:add_table_copy(schema, node(),ram_copies),
	    % Update with tables
	    mnesia:add_table_copy(server, node(), ram_copies),
%	    mnesia:add_table_copy(deployment, node(), ram_copies),
%	    mnesia:add_table_copy(deployment_spec, node(), ram_copies),
	    mnesia:add_table_copy(passwd, node(), ram_copies),
	    mnesia:add_table_copy(sd, node(), ram_copies),
%	    mnesia:add_table_copy(service_def, node(), ram_copies),
	    mnesia:add_table_copy(vm, node(), ram_copies),
	    mnesia:add_table_copy(log, node(), ram_copies),
	    mnesia:add_table_copy(app_spec, node(), ram_copies),
	    mnesia:add_table_copy(lock, node(), ram_copies),
	    Tables=mnesia:system_info(tables),
	    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES);
	_ ->
	%    io:format(" ~p~n",[{?MODULE,?LINE,node()}]),
	    add_extra_nodes(T)
    end.
