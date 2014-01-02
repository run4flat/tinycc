/*
 * Share preprocessor macros between two contexts.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char first_code[] =
"#define one 1\n"
"#define MAX(first, second) (first < second ? second : first)\n"
"#define swap_int(first, second) do { \\\n"
"    int temp; \\\n"
"    temp = first; \\\n"
"    first = second; \\\n"
"    second = temp; \\\n"
"  } while(0)\n"
"\n"
"int first_one() { return one; }\n"
"int first_max(int l, int r) { return MAX(l, r); }\n"
"int first_swap(int l, int r) { swap_int(l, r); return l; } \n"
;

char second_one_code[] = "int second_one() { return one; }\n";
char second_max_code[] =
"int second_max(int l, int r) { return MAX(l, r); }\n";
char second_swap_code[] =
"int second_swap(int l, int r) { swap_int(l, r); return l; } \n";

int main(int argc, char **argv) {
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	TokenSym_p* my_symtab;
	setup_and_compile_s1(my_symtab, first_code);
	SETUP_SECOND_CALLBACK_DATA;
	
	/* ---- Run sanity tests for first code string ---- */
	
	/* test 'one' macro */
	int (*first_one_ptr)() = tcc_get_symbol(s1, "first_one");
	if (first_one_ptr == NULL) return 1;
	is_i(first_one_ptr(), 1, "sanity check on 'one' macro passes");
	
	/* test 'MAX' macro */
	int (*first_max_ptr)() = tcc_get_symbol(s1, "first_max");
	if (first_max_ptr == NULL) return 1;
	is_i(first_max_ptr(5, 3), 5, "sanity check on first slot of 'MAX' macro passes");
	is_i(first_max_ptr(3, 5), 5, "sanity check on second slot of 'MAX' macro passes");
	
	/* test 'swap_int' macro */
	int (*first_swap_ptr)() = tcc_get_symbol(s1, "first_swap");
	if (first_swap_ptr == NULL) return 1;
	is_i(first_swap_ptr(5, 3), 3, "sanity check on first slot of 'swap_int' macro passes");
	is_i(first_swap_ptr(3, 5), 5, "sanity check on second slot of 'swap_int' macro passes");
	
	/* ---- Check code string that depends on the 'one' macro ---- */
	
	TCCState *s_one = tcc_new();
	setup_and_compile_second_state(s_one, second_one_code);
	int (*second_one_ptr)() = tcc_get_symbol(s_one, "second_one");
	if (second_one_ptr == NULL) return 1;
	is_i(second_one_ptr(), 1, "second_one call works");
	
	/* ---- Check code string that depends on the 'MAX' macro ---- */
	
	TCCState *s_max = tcc_new();
	setup_and_compile_second_state(s_max, second_max_code);
	int (*second_max_ptr)() = tcc_get_symbol(s_max, "second_max");
	if (second_max_ptr == NULL) return 1;
	is_i(second_max_ptr(10, 8), 10, "second_max call works for max in first slot");
	is_i(second_max_ptr(120, 245), 245, "second_max call works for max in second slot");
	
	/* ---- Check code string that depends on the 'swap_int' macro ---- */
	
	TCCState *s_swap = tcc_new();
	setup_and_compile_second_state(s_swap, second_swap_code);
	int (*second_swap_ptr)(int, int) = tcc_get_symbol(s_swap, "second_swap");
	if (second_swap_ptr == NULL) return 1;
	is_i(second_swap_ptr(5, 3), 3, "first slot of 'swap_int' macro passes");
	is_i(second_swap_ptr(3, 5), 5, "second slot of 'swap_int' macro passes");
	
	/* ---- clean up the memory ---- */
	
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s_one);
	tcc_delete(s_max);
	tcc_delete(s_swap);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
