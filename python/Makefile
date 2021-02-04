# ------------------------------------------------
# Python Makefile
# ------------------------------------------------

TARGET		:= puzzle

SRCDIR		:= .
BINDIR   	:= bin

.PHONY: all clean

all: $(BINDIR)/$(TARGET)

clean:
	rm -rf $(BINDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

$(BINDIR)/$(TARGET): | $(BINDIR)
	ln -sf ../$(TARGET).py $(BINDIR)/$(TARGET)
