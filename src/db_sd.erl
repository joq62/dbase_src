-module(db_sd).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_sd.hrl").

-define(TABLE,sd).
-define(RECORD,sd).

%Start Special 


active_apps()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [AppId||{?RECORD,_ServiceId,_ServiceVsn,AppId,_AppVsn,_HostId,_VmId,_Vm}<-Z].

app_spec(AppId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId])),
    [{ServiceId,ServiceVsn,XAppId,XAppVsn,HostId,VmId,VmDir,Vm}||{?RECORD,ServiceId,ServiceVsn,XAppId,XAppVsn,HostId,VmId,VmDir,Vm}<-Z].

host(HostId)->
     Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.host_id==HostId])),
    [{ServiceId,ServiceVsn,AppId,AppVsn,XHostId,VmId,VmDir,Vm}||{?RECORD,ServiceId,ServiceVsn,AppId,AppVsn,XHostId,VmId,VmDir,Vm}<-Z].

get(ServiceId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.service_id==ServiceId])),
    [Vm||{?RECORD,_ServiceId,_ServiceVsn,_AppId,_AppVsn,_HostId,_VmId,_VmDir,Vm}<-Z].

get(ServiceId,ServiceVsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.service_id==ServiceId,
		     X#?RECORD.service_vsn==ServiceVsn])),
    [Vm||{?RECORD,_ServiceId,_ServiceVsn,_AppId,_AppVsn,_HostId,_VmId,_VmDir,Vm}<-Z].


% End Special

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm}) ->
    create(ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm).
create(ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm) ->
    TimeStamp=erlang:system_time(seconds),
    Record=#?RECORD{service_id=ServiceId,
		    service_vsn=ServiceVsn,
		    app_id=AppId,
		    app_vsn=AppVsn,
		    host_id=HostId,
		    vm_id=VmId,
		    vm_dir=VmDir,
		    vm=Vm,
		    time_stamp=TimeStamp
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).


remove_orphanes(

heartbeat(Service,Node)->
    ServiceId=atom_to_list(Service),
    F=fun()->
	      SdRecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.service_id==ServiceId,
					  X#?RECORD.vm==Node])),
	      case SdRecordList of
		  []->
		      mnesia:abort({error,[eexists,Service,Node]});
		  [SdInfo] ->
		      NewSdInfo=SdInfo#?RECORD{time_stamp=erlang:system_time(seconds)},
		      mnesia:delete_object(SdInfo),
		      mnesia:write(NewSdInfo);
		  Reason->
		      mnesia:abort({error,[Reason,Service,Node]})
	      end 
      end,
    mnesia:transaction(F).

read_all_info() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm,TimeStamp}||{?RECORD,ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm,TimeStamp}<-Z].

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm}||{?RECORD,ServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm,_}<-Z].



read(ServiceId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.service_id==ServiceId])),
    [{XServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,Vm}||{?RECORD,XServiceId,ServiceVsn,AppId,AppVsn,HostId,VmId,Vm}<-Z].

read(ServiceId,ServiceVsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.service_id==ServiceId,
		     X#?RECORD.service_vsn==ServiceVsn])),
    [{QServiceId,QServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm}||{?RECORD,QServiceId,QServiceVsn,AppId,AppVsn,HostId,VmId,VmDir,Vm,_}<-Z].

delete(Id,Vsn,Vm) ->
    F = fun() -> 
		ServiceDiscovery=[X||X<-mnesia:read({?TABLE,Id}),
				     X#?RECORD.service_id==Id,
				     X#?RECORD.service_vsn==Vsn,
				     X#?RECORD.vm==Vm],
		case ServiceDiscovery of
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
