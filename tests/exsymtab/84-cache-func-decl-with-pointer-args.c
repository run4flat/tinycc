/*
 * See how well caching works for preprocessor macros.
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

struct point {
	double x;
	double y;
};

double sq_distance(struct point * a) {
	return a->x*a->x + a->y*a->y;
}

char func_decl_code[] =
"struct point {\n"
"	double x;\n"
"	double y;\n"
"};\n"
"double sq_distance(struct point * a);";

char consuming_code[] =
"int tester() {\n"
"	struct point location;\n"
"	location.x = 3;\n"
"	location.y = 4;\n"
"	double ret = sq_distance(&location);\n"
"	if (24.9 < ret && ret < 25.1) return 25;\n"
"	return ret;\n"
"}\n";

enum {
	TS_TEST_GET_TOK,
	TS_TEST_HAS_DEFINE,
	TS_TEST_HAS_STRUCT,
	TS_TEST_HAS_IDENTIFIER
};

int main(int argc, char **argv) {
	
	/* ---- Compile and cache the first code string ---- */
	
	TCCState *s1 = tcc_new();
	SIMPLE_SETUP(s1);
	tcc_save_extended_symtab(s1);
    if (tcc_compile_string(s1, func_decl_code) == -1) return 1;
	extended_symtab_p my_symtab = tcc_get_extended_symbol_table(s1);
	if (!tcc_serialize_extended_symtab(my_symtab, "func_decl.cache"))
		return 1;
	tcc_delete(s1);
	s1 = NULL;
	tcc_delete_extended_symbol_table(my_symtab);
	my_symtab = NULL;
	pass("Function declaration code string compiled and cached fine");
	
	/* ---- Load the symtab from the cached file ---- */
	
	my_symtab = tcc_deserialize_extended_symtab("func_decl.cache");
	ok(my_symtab != NULL, "Deserialization returned non-null symtab pointer");
	ok(tcc_extended_symtab_test(my_symtab, TS_TEST_HAS_IDENTIFIER, "sq_distance"),
		"'sq_distance' has identifier symbol");
	
	SETUP_SECOND_CALLBACK_DATA();
	
	/* ---- Check code string that depends on the struct definition ---- */
	
	TCCState *s_consume = tcc_new();
	setup_and_compile_second_state(s_consume, consuming_code);
	pass("Consuming code compiles fine");
	tcc_add_symbol(s_consume, "sq_distance", sq_distance);
	relocate_second_state(s_consume);
	int (*tester_ptr)() = tcc_get_symbol(s_consume, "tester");
	if (tester_ptr == NULL) goto FAIL;
	is_i(tester_ptr(), 25, "tester code reports success");
	
	/* ---- clean up the memory ---- */
	
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s_consume);
	remove("func_decl.cache");
	pass("cleanup");
	
	return done_testing();
	
FAIL:
	remove("func_decl.cache");
	return 1;
}
