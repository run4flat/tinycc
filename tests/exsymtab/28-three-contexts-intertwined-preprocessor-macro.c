/*
 * Share preprocessor macros between three contexts. One context will define a
 * simple but nontrivial macro. Another context will define a macro that relies
 * on the first macro. A final context will have both extended symbol tables,
 * and will use the macro defined in the second context.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

// Consuming code must create the identifier called "var"
char first_code[] =
"#define add_var_to(val) (val + var)\n"
;

char second_code[] =
"#define add_foo_and_var_to(val) (foo + add_var_to(val))\n"
;

char third_code[] = 
"int test() {\n"
"	int var = 5;\n"
"	int foo = 10;\n"
"	return add_foo_and_var_to(4);\n"
"}\n";

void my_copy_symtab(TokenSym_p* copied_symtab, void * data) {
	TokenSym_p** my_symtabs = (TokenSym_p**)data;
	if (my_symtabs[0] == 0) my_symtabs[0] = copied_symtab;
	else my_symtabs[1] = copied_symtab;
}

TokenSym_p my_lookup_by_name (char * name, int len, void * data, int is_identifier) {
	/* Simply wrap the testing infrastructure's call appropriately */
	second_callback_data mock;
	mock.second_context = 0;
	TokenSym_p returned;
	TokenSym_p** my_symtabs = (TokenSym_p**)data;
	if (my_symtabs[0] != 0) {
		mock.first_symtab = my_symtabs[0];
		returned = lookup_by_name(name, len, &mock, is_identifier);
		if (returned) return returned;
	}
	if (my_symtabs[1] != 0) {
		mock.first_symtab = my_symtabs[1];
		returned = lookup_by_name(name, len, &mock, is_identifier);
		if (returned) return returned;
	}
	return NULL;
}

TokenSym_p my_lookup_by_number (int tok_id, void * data, int is_identifier) {
	second_callback_data mock;
	mock.second_context = 0;
	TokenSym_p returned;
	TokenSym_p** my_symtabs = (TokenSym_p**)data;
	if (my_symtabs[0] != 0) {
		mock.first_symtab = my_symtabs[0];
		returned = lookup_by_number(tok_id, &mock, is_identifier);
		if (returned) return returned;
	}
	if (my_symtabs[1] != 0) {
		mock.first_symtab = my_symtabs[1];
		returned = lookup_by_number(tok_id, &mock, is_identifier);
		if (returned) return returned;
	}
	return NULL;
}
	

int main(int argc, char **argv) {
	
	TokenSym_p my_symtabs[2] = { 0, 0 };
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s_first = tcc_new();
	SIMPLE_SETUP(s_first);
	tcc_set_extended_symtab_callbacks(s_first, &my_copy_symtab, NULL, NULL, my_symtabs);
    if (tcc_compile_string(s_first, first_code) == -1) return 1;
	if (tcc_relocate(s_first, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("First code string compiled and relocated fine");
	
	TCCState *s_second = tcc_new();
	SIMPLE_SETUP(s_second);
	tcc_set_extended_symtab_callbacks(s_second, &my_copy_symtab,
		&my_lookup_by_name, &my_lookup_by_number, my_symtabs);
    if (tcc_compile_string(s_second, second_code) == -1) return 1;
	if (tcc_relocate(s_second, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Second code string compiled and relocated fine");
	
	TCCState *s_third = tcc_new();
	SIMPLE_SETUP(s_third);
	tcc_set_extended_symtab_callbacks(s_third, NULL,
		&my_lookup_by_name, &my_lookup_by_number, my_symtabs);
    if (tcc_compile_string(s_third, second_code) == -1) return 1;
	if (tcc_relocate(s_third, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Third code string compiled and relocated fine");
	
	/* ---- Check code string that depends on the macro ---- */
	
	int (*gives_nineteen)() = tcc_get_symbol(s_third, "test");
	if (gives_nineteen == NULL) return 1;
	is_i(gives_nineteen(), 19,
		"Mixed up macros produce correct executable code");
	
	/* ---- clean up the memory ---- */
	
	tcc_delete_extended_symbol_table(my_symtabs[0]);
	tcc_delete_extended_symbol_table(my_symtabs[1]);
	tcc_delete(s_first);
	tcc_delete(s_second);
	tcc_delete(s_third);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
