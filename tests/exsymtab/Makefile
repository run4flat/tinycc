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

# 02-ram_tree.test 
TESTS =	\
 01-test-symtab-copy.test \
 10-symtab-compare-func-decl-and-def.test \
 20-two-contexts-define-share.test \
 25-two-contexts-intertwined-preprocessor-macro.test \
 28-three-contexts-intertwined-preprocessor-macro.test \
 30-two-contexts-struct-share.test \
 31-two-contexts-struct-array-share.test \
 32-two-contexts-struct-self-reference.test \
 33-two-contexts-struct-reserved-tokens.test \
 35-two-contexts-vtable-share.test \
 36-two-contexts-typedef-share.test \
 40-two-contexts-func-decl-share.test \
 42-three-contexts-func-share.test \
 47-many-consuming-contexts-func-share.test \
 48-many-consuming-contexts-delete.test \
 49-many-consuming-contexts-early-delete.test \
 50-two-contexts-mixup.test \
 51-many-consuming-contexts-with-constants-early-delete.test \
 60-two-contexts-globals.test \
 61-two-contexts-const-globals.test \
 62-two-contexts-static-globals.test \
 63-two-contexts-enum.test \
 70-symtab-use-check.test \
 80-cache-defines.test \
 81-cache-struct.test \
 82-cache-func-decl.test \
 83-cache-func-decl-with-args.test \
 84-cache-func-decl-with-pointer-args.test

# 02-ram_tree.prove 
PROVES =	\
 01-test-symtab-copy.prove \
 10-symtab-compare-func-decl-and-def.prove \
 20-two-contexts-define-share.prove \
 25-two-contexts-intertwined-preprocessor-macro.prove \
 28-three-contexts-intertwined-preprocessor-macro.prove \
 30-two-contexts-struct-share.prove \
 31-two-contexts-struct-array-share.prove \
 32-two-contexts-struct-self-reference.prove \
 33-two-contexts-struct-reserved-tokens.prove \
 35-two-contexts-vtable-share.prove \
 36-two-contexts-typedef-share.prove \
 40-two-contexts-func-decl-share.prove \
 42-three-contexts-func-share.prove \
 47-many-consuming-contexts-func-share.prove \
 48-many-consuming-contexts-delete.prove \
 49-many-consuming-contexts-early-delete.prove \
 50-two-contexts-mixup.prove \
 51-many-consuming-contexts-with-constants-early-delete.prove \
 60-two-contexts-globals.prove \
 61-two-contexts-const-globals.prove \
 62-two-contexts-static-globals.prove \
 63-two-contexts-enum.prove \
 70-symtab-use-check.prove \
 80-cache-defines.prove \
 81-cache-struct.prove \
 82-cache-func-decl.prove \
 83-cache-func-decl-with-args.prove \
 84-cache-func-decl-with-pointer-args.prove

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

%.dbg: %.c $(top_builddir)/$(LIBTCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBTCC) $(LDFLAGS) -I$(top_srcdir)
	gdb --args $@ lib_path=$(top_srcdir); rm $@

%.valgrind: %.c $(top_builddir)/$(LIBTCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBTCC) $(LDFLAGS) -I$(top_srcdir)
	valgrind --leak-check=yes ./$@ lib_path=$(top_srcdir); rm $@

all test: $(TESTS)

prove: $(PROVES)

curr-test: 02-ram_tree.test
curr-prove:  02-ram_tree.prove
curr-dbg:  02-ram_tree.dbg
