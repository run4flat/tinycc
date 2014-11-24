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

typedef struct {
	TCCState * oldest_context;
	TCCState * middle_context;
	TCCState * current_context;
	TokenSym_p* oldest_symtab;
	TokenSym_p* middle_symtab;
} three_callback_data;

void my_copy_symtab(TokenSym_p* copied_symtab, void * data) {
	three_callback_data* my_data = (three_callback_data*)data;
	if (my_data->oldest_symtab == 0) my_data->oldest_symtab = copied_symtab;
	else my_data->middle_symtab = copied_symtab;
}

/* This takes a second_callback_data pointer and sets its fields based on the
 * current three-callback-data content. We know from the value of middle_symtab
 * whether the middle state has been compiled (nonzero) or not (null), and we
 * use that as our state indicator. */
void setup_mock_data (second_callback_data *mock, void * data) {
	three_callback_data* my_data = (three_callback_data*)data;
	if (my_data->middle_symtab == 0) {
		/* This is the case when the middle context has not yet been established
		 * and it is referring to the original context. */
		mock->second_context = my_data->middle_context;
		mock->first_context = my_data->oldest_context;
		mock->first_symtab = my_data->oldest_symtab;
	}
	else {
		/* This is the case when the middle context has been compiled and copied
		 * and we are working on the third compilation unit. */
		mock->second_context = my_data->current_context;
		mock->first_context = my_data->middle_context;
		mock->first_symtab = my_data->middle_symtab;
	}
}

TokenSym_p my_lookup_by_name (char * name, int len, void * data,
	TokenSym_p**containing_symtab
) {
	/* Simply wrap the testing infrastructure's call appropriately */
	second_callback_data mock;
	setup_mock_data(&mock, data);
	return lookup_by_name(name, len, &mock, containing_symtab);
}

void my_sym_used (char * name, int len, void * data) {
	second_callback_data mock;
	setup_mock_data(&mock, data);
	sym_used(name, len, &mock);
}
	
int main(int argc, char **argv) {
	
	three_callback_data my_data = { 0, 0, 0, 0, 0 };
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s_first = tcc_new();
	my_data.oldest_context = s_first;
	SIMPLE_SETUP(s_first);
	tcc_set_extended_symtab_callbacks(s_first, &my_copy_symtab, NULL, NULL, &my_data);
    if (tcc_compile_string(s_first, first_code) == -1) return 1;
	if (tcc_relocate(s_first, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("First code string compiled and relocated fine");
	
	TCCState *s_second = tcc_new();
	my_data.middle_context = s_second;
	SIMPLE_SETUP(s_second);
	tcc_set_extended_symtab_callbacks(s_second, &my_copy_symtab,
		&my_lookup_by_name, &my_sym_used, &my_data);
    if (tcc_compile_string(s_second, second_code) == -1) return 1;
	if (tcc_relocate(s_second, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Second code string compiled and relocated fine");
	
	TCCState *s_third = tcc_new();
	my_data.current_context = s_third;
	SIMPLE_SETUP(s_third);
	tcc_set_extended_symtab_callbacks(s_third, NULL,
		&my_lookup_by_name, &my_sym_used, &my_data);
    if (tcc_compile_string(s_third, third_code) == -1) return 1;
	if (tcc_relocate(s_third, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Third code string compiled and relocated fine");
	
	/* ---- Check code string that depends on the macro ---- */
	
	int (*gives_nineteen)() = tcc_get_symbol(s_third, "test");
	if (gives_nineteen == NULL) return 1;
	is_i(gives_nineteen(), 19,
		"Mixed up macros produce correct executable code");
	
	/* ---- clean up the memory ---- */
	
	tcc_delete_extended_symbol_table(my_data.oldest_symtab);
	tcc_delete_extended_symbol_table(my_data.middle_symtab);
	tcc_delete(s_first);
	tcc_delete(s_second);
	tcc_delete(s_third);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
