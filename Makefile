all:
	rm -rf ebin/* src/*.beam *.beam;
	rm -rf  *~ */*~  erl_cra*;
	echo Done
doc_gen:
	echo glurk not implemented
test:
	rm -rf ebin/* src/*.beam *.beam;
	rm -rf  *~ */*~  erl_cra*;
	erlc -o ebin src/*.erl;
	erl -pa ebin -s dbase_unit_tests start -sname dbase_test -setcookie unit_test
