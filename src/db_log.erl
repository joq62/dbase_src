-module(db_log).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_log.hrl").



-define(TABLE,log).
-define(RECORD,log).

% Start Special 
severity(Severity)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    L1=[{XSeverity,Vm,Module,Line,Date,Time,DateTime,Text}||{?RECORD,XSeverity,Vm,Module,Line,Date,Time,DateTime,Text}<-Z,
								      Severity==XSeverity],
    L2=sort_by_date(L1),
%    io:format("L2 = ~p~n",[L2]),
    lists:reverse(L2).

latest(0,_)->
    [];
latest(Len,all)->
   lists:sublist(read_all(),Len).
% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({db_log,Vm,Module,Line,Severity,Date,Time,Text})->
    create(Vm,Module,Line,Severity,Date,Time,Text).
create(Vm,Module,Line,Severity,Date,Time,Text)->
    DateTime=calendar:datetime_to_gregorian_seconds({Date,Time}),
    Record=#?RECORD{
	       severity=Severity,
	       vm=Vm,
	       module=Module,
	       line=Line,
	       date=Date,
	       time=Time,
	       datetime=DateTime,
	       text=Text},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    L1=[{Severity,Vm,Module,Line,Date,Time,DateTime,Text}||{?RECORD,Severity,Vm,Module,Line,Date,Time,DateTime,Text}<-Z],
 %   io:format("L1 = ~p~n",[L1]),
    L2=sort_by_date(L1),
    lists:reverse(L2).


delete(Vm,Module,Line,Severity,Date,Time,Text) ->

    F = fun() -> 
		LogDef=[X||X<-mnesia:read({?TABLE,Severity}),
			       X#?RECORD.severity==Severity,
			       X#?RECORD.vm==Vm,
			       X#?RECORD.module==Module,
			       X#?RECORD.line==Line,
			       X#?RECORD.date==Date,
			       X#?RECORD.time==Time,
			       X#?RECORD.text==Text
			      ],
		case LogDef of
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
sort_by_date([])->
    [];
%sort_by_date([{Severity,Vm,Module,Line,Date,Time,DateTime,Text}|T]) ->
%    sort_by_date([{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}||{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}<-T,
%									     XDateTime=<DateTime])
%	++[{Severity,Vm,Module,Line,Date,Time,Text}]++
%	sort_by_date([{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}||{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}<-T,
%										 XDateTime>DateTime]).

sort_by_date([{Severity,Vm,Module,Line,Date,Time,DateTime,Text}|T]) ->
    lists:append([sort_by_date([{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}||{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}<-T,
									     XDateTime=<DateTime]),
	[{Severity,Vm,Module,Line,Date,Time,Text}],
	sort_by_date([{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}||{XSeverity,XVm,XModule,XLine,XDate,XTime,XDateTime,XText}<-T,
										 XDateTime>DateTime])]).
