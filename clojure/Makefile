# ------------------------------------------------
# Leiningen-Clojure Makefile
# ------------------------------------------------

TARGET		:= puzzle
VERSION		:= 0.1.0-SNAPSHOT-standalone

SRCDIR		:= src/puzzle
LEIN_BINDIR	:= target
BINDIR   	:= bin

SOURCES		:= $(wildcard $(SRCDIR)/*.clj)
JARS		:= $(LEIN_BINDIR)/$(TARGET)-$(VERSION).jar
BINARIES	:= $(BINDIR)/$(TARGET)

.PHONY: all clean format

all: $(BINARIES)

clean:
	rm -rf $(BINDIR) $(LEIN_BINDIR)

format:
	lein cljfmt fix

$(JARS): $(SOURCES)
	lein uberjar

$(BINDIR):
	mkdir -p $(BINDIR)

$(BINARIES): $(JARS) | $(BINDIR)
	JAR_ABS=`realpath $<` && printf "#!/bin/bash\njava -jar $$JAR_ABS\n" > $@
	chmod +x $@
