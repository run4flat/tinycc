/*
 * Declare a global variable in one context and access it in another context.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char first_code[] =
"static unsigned int test_var = 42;\n"
"unsigned int get_test_var () {\n"
"    return test_var;\n"
"}\n"
;

char second_code[] =
"unsigned int indirect_get() {\n"
"    return get_test_var();\n"
"}\n"
"unsigned int direct_get() {\n"
"    return test_var;\n"
"}\n"
;

int main(int argc, char **argv) {
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	extended_symtab_p my_symtab;
	setup_and_compile_s1(my_symtab, first_code);
	SETUP_SECOND_CALLBACK_DATA();
	
	/* ---- Make sure the getter and setter actually modify global state ---- */
	unsigned int (*first_get)() = tcc_get_symbol(s1, "get_test_var");
	is_i(first_get(), 42, "first_set/first_get work");
	
	/* ---- Compile code string that depends on the function and global variable ---- */
	
	TCCState *s2 = tcc_new();
	setup_and_compile_second_state(s2, second_code);
	relocate_second_state(s2);
	
	/* ---- Check indirect getter and setter ---- */
	unsigned int (*indirect_get)() = tcc_get_symbol(s2, "indirect_get");
	is_i(indirect_get(), 42, "indirect_get seems to work");
	
	/* ---- Check direct getter and setter ---- */
	unsigned int (*direct_get)() = tcc_get_symbol(s2, "direct_get");
	is_i(direct_get(), 42, "direct_get seems to work");
	
	/* ---- clean up the memory ---- */
	
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s2);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
