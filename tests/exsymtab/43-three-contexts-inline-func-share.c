/*
 * Define an inline function in one context, use in a different context.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char def_code[] =
"inline int fib(int n)\n"
"{\n"
"    if (n <= 2)\n"
"        return 1;\n"
"    else\n"
"        return fib(n-1) + fib(n-2);\n"
"}\n"
;

char using_code[] =
"int fib_of_5() {\n"
"    return fib(5);\n"
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
	int (*fib_of_5_ptr)() = tcc_get_symbol(s2, "fib_of_5");
	if (fib_of_5_ptr == NULL) return -1;
	pass("Found fib_of_5 function pointer");

	/* ---- Make sure the function invocation gives the right answer ---- */
	
	is_i(fib_of_5_ptr(), 5, "Fibonaci function call works");
	
	/* ---- Cleanup ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s2);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
