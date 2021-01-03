-module(db_app_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_app_spec.hrl").



-define(TABLE,app_spec).
-define(RECORD,app_spec).

all_app_specs()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [AppId||{?RECORD,AppId,_Vsn,_Type,_Directives,_Services}<-Z].

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,AppId,Vsn,Type,Directives,Services})->
    create(AppId,Vsn,Type,Directives,Services);
create(X) ->
    io:format("X= ~p~n",[{X,?MODULE,?LINE}]),
    X=glurk.
create(AppId,Vsn,Type,Directives,Services)->
    Record=#?RECORD{ app_id=AppId,
		     vsn=Vsn,
		     type=Type,
		     directives=Directives,
		     services=Services},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{AppId,AppVsn,Type,Directives,Services}||{?RECORD,AppId,AppVsn,Type,Directives,Services}<-Z].



read(AppId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.app_id==AppId])),
    [{XAppId,AppVsn,Type,Directives,Services}||{?RECORD,XAppId,AppVsn,Type,Directives,Services}<-Z].

read(AppId,AppVsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.app_id==AppId,
		     X#?RECORD.vsn==AppVsn])),
    [{XAppId,XAppVsn,Type,Directives,Services}||{?RECORD,XAppId,XAppVsn,Type,Directives,Services}<-Z].

delete(AppId,AppVsn) ->

    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,AppId}),
			       X#?RECORD.app_id==AppId,
			       X#?RECORD.vsn==AppVsn],
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
