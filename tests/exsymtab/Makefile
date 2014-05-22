TOP = ../..
include $(TOP)/Makefile
VPATH = $(top_srcdir)/tests/exsymtab

TCCFLAGS = -B$(TOP) -I$(top_srcdir)/include
ifdef CONFIG_WIN32
 TCCFLAGS = -B$(top_srcdir)/win32 -I$(top_srcdir)/include -L$(TOP)
endif

ifeq ($(TARGETOS),Darwin)
 CFLAGS += -Wl,-flat_namespace,-undefined,warning
 CFLAGS += -ggdb
 TCCFLAGS += -D_ANSI_SOURCE
 export MACOSX_DEPLOYMENT_TARGET:=10.2
endif

TCC = $(TOP)/tcc $(TCCFLAGS)

TESTS =	\
 01-test-symtab-copy.test \
 20-two-contexts-define-share.test \
 25-two-contexts-intertwined-preprocessor-macro.test \
 30-two-contexts-struct-share.test \
 35-two-contexts-vtable-share.test \
 40-two-contexts-func-decl-share.test \
 42-three-contexts-func-share.test \
 45-two-contexts-func-share.test \
 47-many-consuming-contexts-func-share.test \
 48-many-consuming-contexts-delete.test \
 49-many-consuming-contexts-early-delete.test \
 50-two-contexts-mixup.test


# some tests do not pass on all platforms, remove them for now
#ifeq ($(TARGETOS),Darwin)
#  TESTS := $(filter-out test_name,$(TESTS))
#endif
#ifdef CONFIG_WIN32
#  TESTS := $(filter-out test_name test_name,$(TESTS))
#endif

%.prove: %.c $(top_builddir)/$(LIBTCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBTCC) $(LDFLAGS) -I$(top_srcdir)
	prove -v --exec '' ./$@ :: lib_path=$(top_srcdir); rm $@

%.test: %.c $(top_builddir)/$(LIBTCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBTCC) $(LDFLAGS) -I$(top_srcdir)
	./$@ lib_path=$(top_srcdir); rm $@

all test: $(TESTS)
