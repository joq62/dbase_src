-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_deployment.hrl").


-define(TABLE,deployment).
-define(RECORD,deployment).

% Start Special



update_status(DepSpecId,DepSpecVsn,NewStartResult)->
    F = fun() -> 
		Deployment=[X||X<-mnesia:read({?TABLE,DepSpecId}),
			       X#?RECORD.deployment_spec_id==DepSpecId,
			       X#?RECORD.deployment_spec_vsn==DepSpecVsn],
		case Deployment of
		    []->
			io:format("CurrentRecord = ~p~n",[{?MODULE,?LINE,[]}]),
			mnesia:abort(?TABLE);
		    [CurrentRecord]->
		%	io:format("CurrentRecord = ~p~n",[{?MODULE,?LINE,CurrentRecord}]),
			NewRecord=CurrentRecord#?RECORD{start_result=NewStartResult},
		%	io:format("NewRecord = ~p~n",[{?MODULE,?LINE,NewRecord}]),
			mnesia:write(NewRecord)
		end
	end,
    io:format("F= ~p~n",[{?MODULE,?LINE,F}]),
    mnesia:transaction(F). 
    
% End Special 


create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,DepSpecId,DepSpecVsn,Date,Time,StartResult}) ->
    create(DepSpecId,DepSpecVsn,Date,Time,StartResult).
create(DepSpecId,DepSpecVsn,Date,Time,StartResult) ->
    Record=#?RECORD{
		    deployment_spec_id=DepSpecId,
		    deployment_spec_vsn=DepSpecVsn,
		    date=Date,
		    time=Time,
		    start_result=StartResult
		   },

    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{DepSpecId,DepSpecVsn,Date,Time,StartResult}||{?RECORD,DepSpecId,DepSpecVsn,Date,Time,StartResult}<-Z].


read(DepSpecId,DepSpecVsn)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.deployment_spec_id==DepSpecId,
		     X#?RECORD.deployment_spec_vsn==DepSpecVsn])),
    [{ZDepSpecId,ZDepSpecVsn,Date,Time,XStartResult}||{?RECORD,ZDepSpecId,ZDepSpecVsn,Date,Time,XStartResult}<-Z].

delete(DepSpecId,DepSpecVsn) ->
    F = fun() -> 
		Deployment=[X||X<-mnesia:read({?TABLE,DepSpecId}),
			       X#?RECORD.deployment_spec_id==DepSpecId,
			       X#?RECORD.deployment_spec_vsn==DepSpecVsn],
		case Deployment of
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
