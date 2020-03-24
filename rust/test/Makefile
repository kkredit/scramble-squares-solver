# ------------------------------------------------
# Generic Rust Makefile
# ------------------------------------------------

SRCDIR   := .
BINDIR   := bin

SOURCES  := $(wildcard $(SRCDIR)/*.rs)
BINARIES := $(patsubst $(SRCDIR)/%.rs,$(BINDIR)/%,$(SOURCES))
DEBUG_BINARIES := $(patsubst $(SRCDIR)/%.rs,$(BINDIR)/%_debug,$(SOURCES))

.PHONY: all clean debug

all: $(BINARIES)
debug: $(DEBUG_BINARIES)

clean:
	rm -rf $(BINDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

$(BINARIES): $(BINDIR)/% : $(SRCDIR)/%.rs | $(BINDIR)
	rustc -o $@ $<

$(DEBUG_BINARIES): $(BINDIR)/%_debug : $(SRCDIR)/%.rs | $(BINDIR)
	rustc -g -o $@ $<
