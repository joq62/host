all:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf *_a;
	rm -rf *.dir;
	rm -rf _build;
	rm -rf logs;
	rm -rf ebin
	rm -rf rebar.lock;
#	mkdir ebin;		
	rebar3 compile;	
#	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
#	git add -f *;
	git add *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
no_ebin_commit:
	#INFO: no_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf catalog;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	# copy production rebar.config from src
	rm -f rebar.config;
	cp src/rebar.config rebar.config;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	rm -rf ebin;
	rebar3 edoc;
#	git add *;
#	git add -f *;
#	git commit -m $(m);
#	git push;
	git status
	#INFO: no_ebin_commit ENDED SUCCESSFUL
with_ebin_commit:
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf catalog;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	# copy production rebar.config from src
	rm -f rebar.config;
	cp src/rebar.config rebar.config;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
#	rm -rf ebin;
#	git add *;
	git status
	#INFO: with_ebin_commit ENDED SUCCESSFUL
build:
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf doc;
	rm -rf catalog;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf catalog;
	rm -rf *_a;
	# copy production rebar.config from src
	rm -f rebar.config;
	cp src/rebar.config rebar.config;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	rm -rf ebin;
	#INFO: build ENDED SUCCESSFUL
clean:
	#INFO: clean STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf catalog;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	# copy production rebar.config from src
	rm -f rebar.config;
	cp src/rebar.config rebar.config;
	#INFO: clean ENDED SUCCESSFUL
eunit: 
	#INFO: eunit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf catalog;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	# copy local test  test_rebar.config from test
	#rm -f rebar.config;
	#cp src/rebar.config rebar.config;
	#INFO: Creating eunit test code using test_ebin dir;
	mkdir test_ebin;
	cp test/*.app test_ebin;
	#rm test/dependent_apps.erl;
	#cp /home/joq62/erlang/dev_support/dependent_apps.erl test;
	erlc -I include -I /home/joq62/erlang/include -o test_ebin test/*.erl;
	#INFO: Creating Common applications needed for testing
	#INFO: Creating log
	#rm -rf log;
	#git clone https://github.com/joq62/log.git log;
	#erlc -I log/include -I include -I /home/joq62/erlang/include -o test_ebin log/src/*.erl;
	#cp log/src/log.app.src test_ebin/log.app;
	#INFO: Creating resource_discovery
	#rm -rf resource_discovery;
	#git clone https://github.com/joq62/resource_discovery.git resource_discovery;
	#erlc -I resource_discovery/include -I include -I /home/joq62/erlang/include -o test_ebin resource_discovery/src/*.erl;
	#cp resource_discovery/src/rd.app.src test_ebin/rd.app;
	#INFO: Creating etcd 
	#rm -rf etcd;
	#git clone https://github.com/joq62/etcd.git etcd;
	#erlc -I etcd/include -I include -I /home/joq62/erlang/include -o test_ebin etcd/src/*.erl;
	#cp etcd/src/etcd.app.src test_ebin/etcd.app;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	#INFO: Starts the eunit testing .................
	erl -pa ebin -pa priv -pa test_ebin\
	    -sname catalog_a\
	    -run $(m) start\
	    -setcookie a
local_eunit: 
	#INFO: eunit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	# copy production rebar.config from src
	rm -f rebar.config;
	cp test/test_rebar.config rebar.config;
	#INFO: Creating eunit test code using test_ebin dir;
	mkdir test_ebin;
	cp test/*.app test_ebin;
	#rm test/dependent_apps.erl;
	#cp /home/joq62/erlang/dev_support/dependent_apps.erl test;
	erlc -I include -I /home/joq62/erlang/include -o test_ebin test/*.erl;
	#INFO: Creating Common applications needed for testing
	#INFO: Creating ssh_service
	#rm -rf log;
	#git clone https://github.com/joq62/log.git log;
	erlc -I ../ssh_service/include -I include -I /home/joq62/erlang/include -o test_ebin ../ssh_service/src/*.erl;
	cp ../ssh_service/src/ssh_service.app.src test_ebin/ssh_service.app;
	#INFO: Creating log
	#rm -rf log;
	#git clone https://github.com/joq62/log.git log;
	erlc -I ../log/include -I include -I /home/joq62/erlang/include -o test_ebin ../log/src/*.erl;
	cp ../log/src/log.app.src test_ebin/log.app;
	#INFO: Creating resource_discovery
	#rm -rf resource_discovery;
	#git clone https://github.com/joq62/resource_discovery.git resource_discovery;
	erlc -I ../resource_discovery/include -I include -I /home/joq62/erlang/include -o test_ebin ../resource_discovery/src/*.erl;
	cp ../resource_discovery/src/rd.app.src test_ebin/rd.app;
	#INFO: Creating etcd 
	#rm -rf etcd;
	#git clone https://github.com/joq62/etcd.git etcd;
	erlc -I ../etcd/include -I include -I /home/joq62/erlang/include -o test_ebin ../etcd/src/*.erl;
	cp ../etcd/src/etcd.app.src test_ebin/etcd.app;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	#INFO: Starts the eunit testing .................
	erl -pa ebin -pa priv -pa test_ebin\
	    -sname control_a\
	    -run $(m) start\
	    -setcookie a
