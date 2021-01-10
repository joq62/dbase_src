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
test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin/*;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
#	Common service
	erlc -o ebin ../../services/common_src/src/*.erl;
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
