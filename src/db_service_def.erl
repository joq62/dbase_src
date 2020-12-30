-module(db_service_def).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_service_def.hrl").



-define(TABLE,service_def).
-define(RECORD,service_def).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,SpecId,ServiceId,ServiceVsn,StartCmd,GitPath})->
    create(SpecId,ServiceId,ServiceVsn,StartCmd,GitPath).
create(SpecId,ServiceId,ServiceVsn,StartCmd,GitPath)->
    Record=#?RECORD{
		    spec_id=SpecId,
		    service_id=ServiceId,
		    service_vsn=ServiceVsn,
		    start_cmd=StartCmd,
		    gitpath=GitPath},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{SpecId,ServiceId,ServiceVsn,StartCmd,GitPath}||{?RECORD,SpecId,ServiceId,ServiceVsn,StartCmd,GitPath}<-Z].



read(SpecId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.spec_id==SpecId])),
    [{XSpecId,ServiceId,ServiceVsn,StartCmd,GitPath}||{?RECORD,XSpecId,ServiceId,ServiceVsn,StartCmd,GitPath}<-Z].

delete(SpecId) ->
    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,SpecId}),
			    X#?RECORD.spec_id==SpecId],
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
