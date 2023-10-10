DOTENV := $(shell grep -v '^\#' .env)

DEV_IMAGE_TAG = $(TEST_CONTAINER_NAME)-dev
DEV_IMAGE_ID = $(file < .image.dev)

DOCKER ?= docker
DOCKERCOMPOSE ?= docker-compose
DOCKERCOMPOSE_W_ENV = DEV_IMAGE_TAG=$(DEV_IMAGE_TAG) $(DOCKERCOMPOSE)
REBAR ?= rebar3
TEST_CONTAINER_NAME ?= testrunner
RELEASE = _build/default/rel/registry_bench/bin/registry_bench

all: compile

dev-image: .image.dev

.image.dev: Dockerfile.dev .env
	env $(DOTENV) $(DOCKERCOMPOSE_W_ENV) build $(TEST_CONTAINER_NAME)
	$(DOCKER) image ls -q -f "reference=$(DEV_IMAGE_ID)" | head -n1 > $@

clean-dev-image:
    ifneq ($(DEV_IMAGE_ID),)
        $(DOCKER) image rm -f $(DEV_IMAGE_TAG)
        rm .image.dev
    endif

DOCKER_WC_OPTIONS := -v $(PWD):$(PWD) --workdir $(PWD)
DOCKER_WC_EXTRA_OPTIONS ?= --rm
DOCKER_RUN = $(DOCKER) run -t $(DOCKER_WC_OPTIONS) $(DOCKER_WC_EXTRA_OPTIONS)

DOCKERCOMPOSE_RUN = $(DOCKERCOMPOSE_W_ENV) run --rm $(DOCKER_WC_OPTIONS)

wdeps-%: dev-image
	$(DOCKERCOMPOSE_RUN) -T $(TEST_CONTAINER_NAME) make $(if $(MAKE_ARGS),$(MAKE_ARGS) $*,$*); \
	res=$$?; \
	$(DOCKERCOMPOSE_W_ENV) down; \
	exit $$res

# Rebar tasks

rebar-shell:
	$(REBAR) shell

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

lint:
	$(REBAR) lint

check-format:
	$(REBAR) fmt -c

dialyze:
	$(REBAR) as test dialyzer

release:
	$(REBAR) as prod release

eunit:
	$(REBAR) eunit --cover

common-test:
	$(REBAR) ct --cover

common-test.%: apps/hellgate/test/hg_%_tests_SUITE.erl
	$(REBAR) ct --cover --suite=$^ $(if $(CT_CASE),--case=$(strip $(CT_CASE)))

cover:
	$(REBAR) covertool generate

format:
	$(REBAR) fmt -w

clean:
	$(REBAR) clean

distclean: clean-build-image
	rm -rf _build

test: eunit common-test

cover-report:
	$(REBAR) cover
