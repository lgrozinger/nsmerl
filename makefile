.SUFFIXES: .erl .beam .yrl

ERL_SRC_DIR = ./src
ERL_INCLUDE_DIR = ./include
EBIN_DIR = ./ebin
ERLC_FLAGS = -o ${EBIN_DIR} -I ${ERL_SRC_DIR} -I ${ERL_INCLUDE_DIR}

# A complete list of modules to compile
MODULES = nsmops list_misc dsl_scan dsl_parse dsl_transform erl_tree dslc

# A complete list of test suites
TST_SUITES = santoro_ops_SUITE

# test variables
TST_DIR = ./test
TST_LOG_DIR = ./test/runs
TST_FAIL_MSG = TEST FAILURE... see the test logs for details: ${TST_LOG_DIR}
TST_PASS_MSG = Test ok... see the test logs for details: ${TST_LOG_DIR}

# Command for the erlang shell
ERL = erl -boot start_clean

# How to make a .beam file from .erl
%.beam : ${ERL_SRC_DIR}/*/%.erl
	erlc ${ERLC_FLAGS} $<

# How to make a .beam file from .yrl
%.beam : ${ERL_SRC_DIR}/*/%.yrl
	erlc -W -o ${<D} -- $<
	erlc ${ERLC_FLAGS} ${<D}/${*F}.erl

# compile the complete list of modules
all: compile

compile: ${MODULES:%=%.beam}

test: compile
	mkdir -p ${TST_LOG_DIR}
	@ct_run -include ${ERL_SRC_DIR} -include ${ERL_INCLUDE_DIR} -pz ${EBIN_DIR} -dir ${TST_DIR} -logdir ${TST_LOG_DIR} && echo ${TST_PASS_MSG} || echo ${TST_FAIL_MSG}

clean:
	rm -rf ${EBIN_DIR}/*
	rm -rf ${TST_LOG_DIR}/*
	find . -name 'dsl_parse.erl' -delete

clean_tests:
	rm -rf ${TST_LOG_DIR}
	rm -rf ${TST_LOG_DIR}
	rm -rf ${TST_DIR}/*.beam

# cleans ebin folder, test run logs, and beam files for test suites
clean_all: clean clean_tests

type: compile
	dialyzer -r ${ERL_SRC_DIR} -I ${ERL_SRC_DIR} --src --verbose --statistics

gen_tags:
	find . -name '*.[he]rl' -print | etags -
