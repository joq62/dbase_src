all:
#	Common service
	erlc -o ebin ../../services/common_src/src/*.erl;
#	service
	erlc -o ebin src/*.erl;
#	test application
	cp test_src/*.app test_ebin;
	erlc -o test_ebin test_src/*.erl;
	rm -rf ebin/* src/*.beam *.beam  test_src/*.beam test_ebin/*;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
	echo Done
doc_gen:
	echo glurk not implemented
log_terminal:
	rm -rf test_ebin/* src/*.beam *.beam;
	rm -rf  *~ */*~  erl_cra*;
#	common
	erlc -o test_ebin ../common_src/src/*.erl;
	erlc -o test_ebin ../terminal_src/src/terminal.erl;
	erl -pa test_ebin -s terminal start -sname log_terminal -setcookie abc
alert_ticket_terminal:
#	common
	erlc -o test_ebin ../common_src/src/*.erl;
	erlc -o test_ebin ../terminal_src/src/terminal.erl;
	erl -pa test_ebin -s terminal start -sname alert_ticket_terminal -setcookie abc
test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin/*;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
#	Common service
	erlc -o ebin ../../services/common_src/src/*.erl;
#	Common log
	erlc -o ebin ../../services/log_src/src/*.erl;
#	service
	erlc -o ebin src/*.erl;
#	test application
	cp test_src/*.app test_ebin;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
	    -setcookie abc\
	    -sname test_dbase\
	    -run dbase_unit_test start_test test_src/test.config
stop:
	erl_call -a 'rpc call [master@c0 init stop []]' -sname master -c abc;
	erl_call -a 'rpc call [master@c1 init stop []]' -sname master -c abc;
	erl_call -a 'rpc call [master@c2 init stop []]' -sname master -c abc
