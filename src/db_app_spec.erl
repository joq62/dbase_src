-module(db_app_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_app_spec.hrl").



-define(TABLE,app_spec).
-define(RECORD,app_spec).


create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create([?MODULE,{app_id,AppId},{app_vsn,AppVsn},{type,Type},
	{host,Host},{vm_id,VmId},{vm_dir,VmDir},{cookie,Cookie},
	{services,Services}])->

    create(AppId,AppVsn,Type,Host,VmId,VmDir,Cookie,Services);
create(X) ->
    io:format("X= ~p~n",[{X,?MODULE,?LINE}]),
    X={error,["unmatched signal",X,?MODULE,?LINE]}.
create(AppId,AppVsn,Type,Host,VmId,VmDir,Cookie,Services)->
  %  io:format("create(AppId,Vsn,Type,Directives,AppEnvs,Services) = ~p~n",[{AppId,Vsn,Type,Directives,AppEnvs,Services,?MODULE,?LINE}]),
    Record=#?RECORD{app_id=AppId,
		    app_vsn=AppVsn,
		    type=Type,
		    host=Host,
		    vm_id=VmId,
		    vm_dir=VmDir,
		    cookie=Cookie,
		    services=Services},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

%% Unique
all_app_specs()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [AppId||{?RECORD,AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,_Services}<-Z].

host(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [Result|_]=[Host||{?RECORD,_AppId,_AppVsn,_Type,Host,_VmId,_VmDir,_Cookie,_Services}<-Z],
    Result.
vm_id(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [Result|_]=[VmId||{?RECORD,_AppId,_AppVsn,_Type,_Host,VmId,_VmDir,_Cookie,_Services}<-Z],
    Result.
vm_dir(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [Result|_]=[VmDir||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,VmDir,_Cookie,_Services}<-Z],
    Result.
cookie(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [Result|_]=[Cookie||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,Cookie,_Services}<-Z],
    Result.

services(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    AllServices=[Services||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,Services}<-Z],
    AllServices.

 % Assume only one service per application 
service_id(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [[AllServices|_]]=[Services||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,Services}<-Z],
    
    Result=case lists:keyfind(service_id,1,AllServices) of
	       false->
		   {error,[eexists,AppId,'or service_id']};
	       {service_id,ServiceId} ->
		   ServiceId
	   end,
    Result.
service_vsn(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [[AllServices|_]]=[Services||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,Services}<-Z],
    
    Result=case lists:keyfind(service_vsn,1,AllServices) of
	       false->
		   {error,[eexists,AppId,'or service_vsn']};
	       {service_vsn,ServiceVsn} ->
		   ServiceVsn
	   end,
    Result.


git_path(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [[AllServices|_]]=[Services||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,Services}<-Z],
    
    Result=case lists:keyfind(git_path,1,AllServices) of
	       false->
		   {error,[eexists,AppId,'or git_path']};
	       {git_path,GitPath} ->
		   GitPath
	   end,
    Result.

start_cmd(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [[AllServices|_]]=[Services||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,Services}<-Z],
    
    Result=case lists:keyfind(start_cmd,1,AllServices) of
	       false->
		   {error,[eexists,AppId,'or git_path']};
	       {start_cmd,StartCmd} ->
		   StartCmd
	   end,
    Result.

app_env_vars(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [[AllServices|_]]=[Services||{?RECORD,_AppId,_AppVsn,_Type,_Host,_VmId,_VmDir,_Cookie,Services}<-Z],
    
    Result=case lists:keyfind(env_vars,1,AllServices) of
	       false->
		   {error,[eexists,AppId,'or git_path']};
	       {env_vars,EnvVars} ->
		   EnvVars
	   end,
    Result.


%% Standard CRUD

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{AppId,AppVsn,Type,Host,VmId,VmDir,Cookie,Services}||{?RECORD,AppId,AppVsn,Type,Host,VmId,VmDir,Cookie,Services}<-Z].



read(AppId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.app_id==AppId])),
    [{XAppId,AppVsn,Type,Host,VmId,VmDir,Cookie,Services}||{?RECORD,XAppId,AppVsn,Type,Host,VmId,VmDir,Cookie,Services}<-Z].

read(AppId,AppVsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId,
		     X#?RECORD.app_vsn==AppVsn])),
    [{XAppId,XAppVsn,Type,Type,Host,VmId,VmDir,Cookie,Services}||{?RECORD,XAppId,XAppVsn,Type,Host,VmId,VmDir,Cookie,Services}<-Z].

delete(AppId,AppVsn) ->

    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,AppId}),
			       X#?RECORD.app_id==AppId,
			       X#?RECORD.app_vsn==AppVsn],
		case ServiceDef of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
