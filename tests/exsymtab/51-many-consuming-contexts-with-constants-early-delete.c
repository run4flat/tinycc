/*
 * Define a function once, use it repeatedly, deleting tcc states as
 * soon as possible.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

int fib (int n);
char buffer[200];
int fib (int n) {
    if (n <= 2)
        return 1;
    else
        return fib(n-1) + fib(n-2);
}

char def_code[] =
"int fib(int n);\n"
"int fib_of_5() { return fib(5); }\n"
;

char first_code[] =
"int twice_fib_of_5()\n"
"{\n"
"    return 2 * fib_of_5();\n"
"}\n"
;

char second_code[] =
"int thrice_fib_of_5() {\n"
"    return 3 * fib_of_5();\n"
"}\n"
;

int main(int argc, char **argv) {

	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	SIMPLE_SETUP(s1);
	
	/* Code taken from test_setup.h and modified to work with my own relocation
	 * point. */
	tcc_save_extended_symtab(s1);
    if (tcc_compile_string(s1, def_code) == -1) return 1;
    tcc_add_symbol(s1, "fib", fib);
    int def_code_size = tcc_relocate(s1, 0);
    void * code = malloc(def_code_size);
    if (code == NULL) return 1;
	if (tcc_relocate(s1, code) == -1) return 1;
	extended_symtab_p my_symtab = tcc_get_extended_symbol_table(s1);
	pass("First code string compiled and relocated fine");
	
	/* ---- Get the fib_of_5 function and evaluate it ---- */
	
	int (*fib5)() = tcc_get_symbol(s1, "fib_of_5");
	if (fib5 == NULL) return -1;
	pass("Found fib_of_5");
	is_i(fib5(), 5, "Calling fib_of_5 from defining compiler context works");
	
	/* ---- Delete the original tcc state ---- */
	
	tcc_delete(s1);
	
	/* ---- Compile the second string ---- */
	
	SETUP_SECOND_CALLBACK_DATA();
	TCCState *s_first = tcc_new();
	setup_and_compile_second_state(s_first, first_code);
	relocate_second_state(s_first);
	
	/* ---- Check the functionality ---- */
	
	/* Retrieve twice_fib_of_5 */
	int (*fib5x2)() = tcc_get_symbol(s_first, "twice_fib_of_5");
	if (fib5x2 == NULL) return -1;
	pass("Found twice_fib_of_5 function pointer");
	is_i(fib5x2(), 10, "Dependent function call works");
	
	/* ---- Delete the first dependent state ---- */
	
	tcc_delete(s_first);
	
	/* ---- Compile the third string ---- */
	
	TCCState *s_second = tcc_new();
	setup_and_compile_second_state(s_second, second_code);
	relocate_second_state(s_second);
	
	/* ---- Check the functionality ---- */
	
	int (*fib5x3)() = tcc_get_symbol(s_second, "thrice_fib_of_5");
	if (fib5x3 == NULL) return -1;
	pass("Found thrice_fib_of_5 function pointer");
	is_i(fib5x3(), 15, "Second dependent fibonaci function call works");
	
	/* ---- Cleanup ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s_second);
	free(code);
	pass("cleanup");
	
	return done_testing();
}
