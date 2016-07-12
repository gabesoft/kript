
all: build

CONF = $(CURDIR)/kript.cabal
DIST = $(CURDIR)/dist
BIN = $(CURDIR)/bin
SRC_LIB = $(shell find $(CURDIR)/src-lib -iname "[^.]*.hs")
SRC_EXE = $(shell find $(CURDIR)/src-exe -iname "[^.]*.hs")
DST = $(shell cat $(CONF) | grep executable | awk '{print $$2}')
OUT = $(shell echo $(DST) | awk '{print "$(DIST)/build/"$$1"/"$$1}')
TEMP = $(shell find $(CURDIR) -name ".\#*")
TEST_OUT = $(DIST)/build/tests/tests
TEST_SRC = $(shell find $(CURDIR)/test -iname "[^.]*.hs")
CABAL = cabal

build: configure $(OUT) bin clean-temp
	@cp $(OUT) $(BIN)/

$(TEST_OUT): $(TEST_SRC) $(SRC_LIB) $(CONF) clean-temp
	@$(CABAL) test

$(OUT): $(SRC_LIB) $(SRC_EXE) $(CONF)
	@$(CABAL) build

clean: clean-temp
	@$(RM) -r $(DIST)

clean-temp:
	@$(RM) $(TEMP)

bin:
	@mkdir -p $(BIN)

install:
	$(CABAL) install --only-dependencies
	$(CABAL) install --enable-tests

sandbox:
	$(CABAL) sandbox init

sandbox-delete:
	$(CABAL) sandbox delete

configure:
	$(CABAL) configure

configure-test:
	$(CABAL) configure --enable-tests

test: $(TEST_OUT)
	@$(TEST_OUT)

repl:
	$(CABAL) repl --ghc-option="-package ghci-pretty"

.PHONY: clean, all, build
