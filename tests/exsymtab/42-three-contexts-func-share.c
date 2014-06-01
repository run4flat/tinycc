/*
 * Share functions between two contexts.
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
"int fib(int n);\n"
;

char second_code[] =
"int fib_of_5() {\n"
"    return fib(5);\n"
"}\n"
"void* get_fib_address() {\n"
"    return &fib;\n"
"}\n"
;

int main(int argc, char **argv) {

	/* ---- Compile the code string with the definition ---- */
	
	TCCState *s_def = tcc_new();
	SIMPLE_SETUP(s_def);
    if (tcc_compile_string(s_def, def_code) == -1) return 1;
	if (tcc_relocate(s_def, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Code string with definition of fib function compiled and relocated fine");
	
	/* ---- Get the Fibonaci function and evaluate it ---- */
	
	int (*fib_def)(int) = tcc_get_symbol(s_def, "fib");
	if (fib_def == NULL) return -1;
	pass("Found fib");
	is_i(fib_def(5), 5, "Calling fib from first compiler context works");
	
	/* ---- Compile the code string with the declaration ---- */
	
	TCCState *s1 = tcc_new();
	TokenSym_p* my_symtab;
	setup_and_compile_s1(my_symtab, first_code);
	SETUP_SECOND_CALLBACK_DATA;
	
	/* ---- Compile the second string ---- */
	
	TCCState *s2 = tcc_new();
	setup_and_compile_second_state(s2, second_code);
	tcc_add_symbol(s2, "fib", fib_def);
	relocate_second_state(s2);
	
	/* ---- Check the function pointer addresses ---- */
	
	/* Is fib in the correct location? */
	void* (*get_fib_address)(void) = tcc_get_symbol(s2, "get_fib_address");
	if (get_fib_address == NULL) return -1;
	pass("Found get_fib_address function pointer");
	int (*fib_from_second)(int) = get_fib_address();
	is_p(fib_from_second, fib_def, "address of fib function in second context is correct");
	
	/* Retrieve fib_of_5 directly */
	int (*fib_of_5_ptr)() = tcc_get_symbol(s2, "fib_of_5");
	if (fib_of_5_ptr == NULL) return -1;
	pass("Found fib_of_5 function pointer");
	isnt_p(fib_of_5_ptr, fib_from_second, "fib_of_5 has different address from fib in second context");

	/* ---- Make sure the function invocation gives the right answer ---- */
	
	if (fib_def == fib_from_second) {
		is_i(fib_of_5_ptr(), 5, "Fibonaci function call works");
	}
	else {
		fail("Cannot test Fibonaci function call");
	}
	
	/* ---- Cleanup ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s_def);
	tcc_delete(s1);
	tcc_delete(s2);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
