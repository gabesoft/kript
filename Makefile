
all: build

CONF = $(CURDIR)/kript.cabal
DIST = $(CURDIR)/dist
BIN = $(CURDIR)/bin
SRC = $(CURDIR)/src/*.hs
DST = $(shell cat $(CONF) | grep executable | awk '{print $$2}')
OUT = $(shell echo $(DST) | awk '{print "$(DIST)/build/"$$1"/"$$1}')
TEMP = $(shell find $(CURDIR) -name ".\#*")
TEST_OUT = $(DIST)/build/tests/tests
TEST_SRC = $(shell find $(CURDIR)/test -iname "[^.]*.hs")
CABAL = cabal

build: $(OUT) bin clean-temp
	@cp $(OUT) $(BIN)/

$(TEST_OUT): $(TEST_SRC) $(SRC) clean-temp
	@$(CABAL) test

$(OUT): $(SRC)
	@$(CABAL) build

clean: clean-temp
	@$(RM) -r $(DIST)

clean-temp:
	@$(RM) $(TEMP)

bin:
	@mkdir -p $(BIN)

install:
	@$(CABAL) install

sandbox:
	@$(CABAL) init sandbox

test: $(TEST_OUT)
	@$(TEST_OUT)

.PHONY: clean, all