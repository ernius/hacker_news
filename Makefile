DOCKER_ARGS = --rm --volume "$(shell pwd)":/hacker_stories --workdir "/hacker_stories" -it
DOCKER = docker run $(DOCKER_ARGS) erlang:20.3

.PHONEY: deps-docker tests-docker run-docker run doc xref tests

deps:
	rebar3 as dev compile

tests:
	rebar3 eunit
	rebar3 ct

run:
	ERL_FLAGS=" -args_file config/vm.args -config config/sys.config" rebar3 as dev shell

doc:
	rebar3 edoc

xref:
	rebar3 xref

deps-docker:
	$(DOCKER) rebar3 as dev compile

tests-docker:
	$(DOCKER) rebar3 eunit
	$(DOCKER) rebar3 ct

run-docker: DOCKER_ARGS += --publish 8000:8000
run-docker:
	$(DOCKER) rebar3 as dev shell
