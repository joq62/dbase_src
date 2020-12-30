-module(db_app_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_app_spec.hrl").



-define(TABLE,app_spec).
-define(RECORD,app_spec).

all_app_specs()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [AppId||{?RECORD,AppId,_Vsn,_Directives,_Services}<-Z].

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,AppId,Vsn,Directives,Services})->
    create(AppId,Vsn,Directives,Services).
create(AppId,Vsn,Directives,Services)->
    Record=#?RECORD{ app_id=AppId,
		     vsn=Vsn,
		     directives=Directives,
		     services=Services},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{AppId,Vsn,Directives,Services}||{?RECORD,AppId,Vsn,Directives,Services}<-Z].



read(AppId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.app_id==AppId])),
    [{XAppId,XVsn,XDirectives,XServices}||{?RECORD,XAppId,XVsn,XDirectives,XServices}<-Z].

read(AppId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId,
		     X#?RECORD.vsn==Vsn])),
    [{XAppId,XVsn,XDirectives,XServices}||{?RECORD,XAppId,XVsn,XDirectives,XServices}<-Z].

delete(Id,Vsn) ->

    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.app_id==Id,X#?RECORD.vsn==Vsn],
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
