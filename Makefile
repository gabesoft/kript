
all: build

CONF = $(CURDIR)/kript.cabal
DIST = $(CURDIR)/dist
BIN = $(CURDIR)/bin
SRC = $(CURDIR)/src/*.hs
DST = $(shell cat $(CONF) | grep executable | awk '{print $$2}')
OUT = $(shell echo $(DST) | awk '{print "$(DIST)/build/"$$1"/"$$1}')
CABAL = cabal

build: $(OUT) bin
	@cp $(OUT) $(BIN)/

$(OUT): $(SRC)
	@$(CABAL) build

clean:
	@$(RM) -r $(DIST)

bin:
	@mkdir -p $(BIN)

install:
	$(CABAL) install

sandbox:
	$(CABAL) init sandbox

.PHONY: clean