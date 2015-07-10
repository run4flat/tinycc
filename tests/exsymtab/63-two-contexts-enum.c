/*
 * Share enum definitions between two contexts.
 */

/* uncomment to enable diagnostic output */
	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char first_code[] =
"enum {\n"
"    foo = 10,\n"
"    bar,\n"
"    baz\n"
"};\n"
;

char second_code[] =
"int get_foo() { return foo; }\n"
"int get_bar() { return bar; }\n"
"int get_baz() { return baz; }\n"
;

int main(int argc, char **argv) {
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	extended_symtab_p my_symtab;
	setup_and_compile_s1(my_symtab, first_code);
	SETUP_SECOND_CALLBACK_DATA();
	
	/* ---- Compile the second compiler context ---- */
	TCCState * s_second = tcc_new();
	setup_and_relocate_second_state(s_second, second_code);
	int (*get_foo_ptr)() = tcc_get_symbol(s_second, "get_foo");
	if (get_foo_ptr == NULL) return 1;
	is_i(get_foo_ptr(), 10, "Second context has correct value for foo");
	int (*get_bar_ptr)() = tcc_get_symbol(s_second, "get_bar");
	if (get_bar_ptr == NULL) return 1;
	is_i(get_bar_ptr(), 11, "Second context has correct value for bar");
	int (*get_baz_ptr)() = tcc_get_symbol(s_second, "get_baz");
	if (get_baz_ptr == NULL) return 1;
	is_i(get_baz_ptr(), 12, "Second context has correct value for baz");
	
	/* ---- clean up the memory ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s_second);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
