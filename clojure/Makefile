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

.PHONY: all clean

all: $(BINARIES)

clean:
	rm -rf $(BINDIR) $(LEIN_BINDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

$(JARS): $(SOURCES)
	lein uberjar

$(BINARIES): $(JARS) | $(BINDIR)
	JAR_ABS=`realpath $<` && printf "#!/bin/bash\njava -jar $$JAR_ABS\n" > $@
	chmod +x $@
