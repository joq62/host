-define(HostSpecDir,"host_specs").
-define(GitPathHostSpecs,"https://github.com/joq62/host_specs.git").
-define(Extension,".host").

-define(TABLE,host).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 hostname,
		 ip,
		 ssh_port,
		 user,
		 passwd,
		 application_config
		}).
