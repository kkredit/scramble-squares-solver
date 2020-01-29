# ------------------------------------------------
# Generic Go Makefile
# ------------------------------------------------

SRCDIR   := .
BINDIR   := bin

SOURCES  := $(wildcard $(SRCDIR)/*.go)
BINARIES := $(patsubst $(SRCDIR)/%.go,$(BINDIR)/%,$(SOURCES))

.PHONY: all clean format fmt

all: $(BINARIES)

clean:
	rm -rf $(BINDIR)

fmt: format
format:
	gofmt -w $(SRCDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

$(BINARIES): $(BINDIR)/% : $(SRCDIR)/%.go | $(BINDIR)
	go build -o $@ $<
