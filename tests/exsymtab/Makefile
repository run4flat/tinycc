TOP = ../..
include $(TOP)/Makefile
VPATH = $(top_srcdir)/tests/exsymtab

TCCFLAGS = -B$(TOP) -I$(top_srcdir)/include
ifdef CONFIG_WIN32
 TCCFLAGS = -B$(top_srcdir)/win32 -I$(top_srcdir)/include -L$(TOP)
endif

ifeq ($(TARGETOS),Darwin)
 CFLAGS += -Wl,-flat_namespace,-undefined,warning
 TCCFLAGS += -D_ANSI_SOURCE
 export MACOSX_DEPLOYMENT_TARGET:=10.2
endif

TCC = $(TOP)/tcc $(TCCFLAGS)

TESTS =	\
 01-test-symtab-copy.test \
 10-link-two-contexts.test


# some tests do not pass on all platforms, remove them for now
#ifeq ($(TARGETOS),Darwin)
#  TESTS := $(filter-out test_name,$(TESTS))
#endif
#ifdef CONFIG_WIN32
#  TESTS := $(filter-out test_name test_name,$(TESTS))
#endif

%.test: %.c $(top_builddir)/$(LIBTCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBTCC) $(LDFLAGS) -I$(top_srcdir)
	./$@ lib_path=$(top_srcdir)
	rm $@

all test: $(TESTS)
