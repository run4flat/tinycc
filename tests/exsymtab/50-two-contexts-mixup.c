/*
 * Define a function in the first context along with a wrapper macro which
 * depends upon a token which will be provided in the second context.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

// The user can call sum_up, providing from and to
// Or, the user can create a variable called "from"
// and then call "sum_to", simply providing the "to" argument
char first_code[] =
"unsigned int sum_up(unsigned int from, unsigned int to) {\n"
"	return to*(to+1)/2 - from*(from-1)/2;\n"
"	\n"
"}\n"
"#define sum_to(to) sum_up(from, to)\n"
;

char second_code[] =
"int simple_sum() {\n"
"	return sum_up(5, 10);\n"
"}\n"
"int default_sum() {\n"
"	int from = 5;\n"
"	return sum_to(10);\n"
"}\n"
;

int main(int argc, char **argv) {
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	extended_symtab_p my_symtab;
	setup_and_compile_s1(my_symtab, first_code);
	SETUP_SECOND_CALLBACK_DATA();
	
	/* ---- Make sure the algorithm is correct ---- */
	int (*sum_up_ptr)(int, int) = tcc_get_symbol(s1, "sum_up");
	is_i(sum_up_ptr(1, 5), 15, "sum_up 1 -> 5 gives correct answer");
	is_i(sum_up_ptr(5, 10), 45, "sum_up 5 -> 10 gives correct answer");
	
	/* ---- Check code string that depends on the function and macro ---- */
	
	TCCState *s2 = tcc_new();
	setup_and_compile_second_state(s2, second_code);
	relocate_second_state(s2);
	
	int (*simple_sum_ptr)() = tcc_get_symbol(s2, "simple_sum");
	if (simple_sum_ptr == NULL) return 1;
	is_i(simple_sum_ptr(), 45, "embedded function calls work (already tested)");
	
	int (*default_sum_ptr)() = tcc_get_symbol(s2, "default_sum");
	if (default_sum_ptr == NULL) return 1;
	is_i(default_sum_ptr(), 45, "macro-wrapped function with default local args");
	
	/* ---- clean up the memory ---- */
	
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s2);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
