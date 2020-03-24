# ------------------------------------------------
# Generic C Makefile
# ------------------------------------------------

CC=gcc
CXX=g++

EXENAME  := puzzle
SRCDIR   := .
BUILDDIR := bin

vpath %.c $(dir $(SRCDIR))
vpath %.cpp $(dir $(SRCDIR))

EXEFILE  := $(BUILDDIR)/$(EXENAME)
SOURCES  := $(wildcard $(SRCDIR)/*.c)
OBJECTS  := $(patsubst %.c,$(BUILDDIR)/%.o,$(patsubst %.cpp,$(BUILDDIR)/%.o,$(notdir $(SOURCES))))
INCLUDES := $(SRCDIR)
INC_DIRS := $(foreach incdir,$(INCLUDES),-I$(incdir))
GCOVGCNO:=$(patsubst %.o,$(BUILDDIR)/%.gcno,$(notdir $(OBJS)))
GCOVGCDA:=$(patsubst %.o,$(BUILDDIR)/%.gcda,$(notdir $(OBJS)))

WARNINGS := \
	-Wall -Wextra -Wpedantic -Werror \
	-Wlogical-op -Waggregate-return -Wfloat-equal -Wcast-align \
	-Wparentheses -Wmissing-braces -Wconversion -Wsign-conversion \
	-Wwrite-strings -Wunknown-pragmas -Wunused-macros \
	-Wnested-externs -Wpointer-arith -Wswitch -Wredundant-decls \
	-Wreturn-type -Wshadow -Wstrict-prototypes -Wunused -Wuninitialized \
	-Wdeclaration-after-statement -Wmissing-prototypes \
	-Wmissing-declarations -Wundef -fstrict-aliasing -Wstrict-aliasing=3 \
	-Wformat=2 -Wsuggest-attribute=pure -Wsuggest-attribute=const

# CFLAGS += -std=c99 -D_POSIX_C_SOURCE=200112L -O0 -ggdb3 $(WARNINGS) $(INC_DIRS)
CFLAGS += -std=c99 -D_POSIX_C_SOURCE=200112L -O2 $(WARNINGS) $(INC_DIRS)
LFLAGS += $(WARNINGS) $(INC_DIRS) -static

default: all
.PHONY: all
all:
	$(MAKE) $(EXEFILE) -j $(shell nproc)

.PHONY: coverage
coverage:
	CFLAGS=--coverage $(MAKE) $(EXEFILE) -j $(shell nproc)
	./$(EXEFILE)
	lcov -c -d . -o $(EXEFILE).info
	genhtml --legend -o $(BUILDDIR)/coveragereport $(EXEFILE).info

$(EXEFILE): $(OBJECTS)
	$(CXX) -o $@ $(CFLAGS) $^ $(LFLAGS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	$(CC) -c -o $@ $(CFLAGS) $< $(LFLAGS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -c -o $@ $(CFLAGS) $< $(LFLAGS)

$(OBJECTS): | $(BUILDDIR)

$(BUILDDIR):
	mkdir $(BUILDDIR)

.PHONY: clean
clean:
	$(RM) $(EXEFILE) $(OBJECTS) $(GCOVGCNO) $(GCOVGCDA) $(EXEFILE).info
	$(RM) -r $(BUILDDIR)/coveragereport
