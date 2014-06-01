/*
 * Define a function once, use it repeatedly, deleting tcc states as
 * soon as possible.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char def_code[] =
"int fib(int n)\n"
"{\n"
"    if (n <= 2)\n"
"        return 1;\n"
"    else\n"
"        return fib(n-1) + fib(n-2);\n"
"}\n"
;

char first_code[] =
"int fib_of_5()\n"
"{\n"
"    return fib(5);\n"
"}\n"
;

char second_code[] =
"int fib_of_10() {\n"
"    return fib(10);\n"
"}\n"
;

int main(int argc, char **argv) {

	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	SIMPEL_SETUP(s1);
	TokenSym_p* my_symtab;
	
	/* Code taken from test_setup.h and modified to work with my own relocation
	 * point. */
	tcc_set_extended_symtab_callbacks(s1, &copy_symtab, NULL, NULL, &my_symtab);
    if (tcc_compile_string(s1, def_code) == -1) return 1;
    int def_code_size = tcc_relocate(s1, 0);
    void * code = malloc(def_code_size);
    if (code == NULL) return 1;
	if (tcc_relocate(s1, code) == -1) return 1;
	pass("First code string compiled and relocated fine");
	
	/* ---- Get the Fibonaci function and evaluate it ---- */
	
	int (*fib_from_def)(int) = tcc_get_symbol(s1, "fib");
	if (fib_from_def == NULL) return -1;
	pass("Found fib");
	is_i(fib_from_def(5), 5, "Calling fib from defining compiler context works");
	DIAG("Address of fib is %p", fib_from_def);
	
	/* ---- Delete the original tcc state ---- */
	
	tcc_delete(s1);
	
	/* ---- Compile the second string ---- */
	
	SETUP_SECOND_CALLBACK_DATA;
	TCCState *s_first = tcc_new();
	setup_and_compile_second_state(s_first, first_code);
	tcc_add_symbol(s_first, "fib", fib_from_def);
	relocate_second_state(s_first);
	
	/* ---- Check the functionality ---- */
	
	/* Retrieve fib_of_5 */
	int (*fib_of_5_ptr)() = tcc_get_symbol(s_first, "fib_of_5");
	if (fib_of_5_ptr == NULL) return -1;
	pass("Found fib_of_5 function pointer");
	is_i(fib_of_5_ptr(), 5, "Fibonaci function call works");
	
	/* ---- Delete the first dependent state ---- */
	
	tcc_delete(s_first);
	
	/* ---- Compile the third string ---- */
	
	TCCState *s_second = tcc_new();
	setup_and_compile_second_state(s_second, second_code);
	tcc_add_symbol(s_second, "fib", fib_from_def);
	relocate_second_state(s_second);
	
	/* ---- Check the functionality ---- */
	
	int (*fib_of_10_ptr)() = tcc_get_symbol(s_second, "fib_of_10");
	if (fib_of_10_ptr == NULL) return -1;
	pass("Found fib_of_10 function pointer");
	is_i(fib_of_10_ptr(), 55, "Fibonaci function call works");
	
	/* ---- Cleanup ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s_second);
	free(code);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
