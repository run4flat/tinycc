/*
 * Share typedefs between two contexts.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char first_code[] =
"typedef int my_type;\n"
;

char second_code[] =
"my_type echo(my_type val) { return val; }\n"
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
	int (*echo)(int) = tcc_get_symbol(s_second, "echo");
	if (echo == NULL) return 1;
	is_i(echo(15), 15, "Second context has correct typedef");
	
	/* ---- clean up the memory ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s_second);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
