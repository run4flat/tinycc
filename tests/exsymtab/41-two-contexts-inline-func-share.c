/*
 * Define an inline function in one context, use in a different context.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char def_code[] =
"inline int add_10(int n)\n"
"{\n"
"    return (n + 10);\n"
"}\n"
;

char using_code[] =
"int five_plus_10() {\n"
"    return(add_10(5));\n"
"}\n"
;

int main(int argc, char **argv) {

	/* ---- Compile the code string with the definition ---- */
	
	TCCState *s1 = tcc_new();
	extended_symtab_p my_symtab;
	setup_and_compile_s1(my_symtab, def_code);
	SETUP_SECOND_CALLBACK_DATA();
	
	/* ---- Compile the second string ---- */
	
	TCCState *s2 = tcc_new();
	setup_and_compile_second_state(s2, using_code);
	relocate_second_state(s2);
	
	/* ---- Check the functionality ---- */
	
	/* Retrieve fib_of_5 directly */
	int (*five_plus_10_ptr)() = tcc_get_symbol(s2, "five_plus_10");
	if (five_plus_10_ptr == NULL) return -1;
	pass("Found five_plus_10 function pointer");

	/* ---- Make sure the function invocation gives the right answer ---- */
	
	is_i(five_plus_10_ptr(), 15, "Function calling an inline function works");
	
	/* ---- Cleanup ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s2);
	pass("cleanup");
	
	return done_testing();
}
