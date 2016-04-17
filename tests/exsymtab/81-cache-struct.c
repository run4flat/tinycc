/*
 * See how well caching works for preprocessor macros.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char struct_code[] =
"struct point {\n"
"    int x;\n"
"    int y;\n"
"};\n"
;

char consuming_code[] =
"int sq_distance_to_pt(struct point * pt) {\n"
"    return (pt->x) * (pt->x) + (pt->y) * (pt->y);\n"
"}\n"
"int tester() {\n"
"       struct point my_point;\n"
"       my_point.x = 5;\n"
"       my_point.y = 3;\n"
"       return sq_distance_to_pt(&my_point);\n"
"}\n";

enum {
    TS_TEST_GET_TOK,
    TS_TEST_HAS_DEFINE,
    TS_TEST_HAS_STRUCT,
    TS_TEST_HAS_IDENTIFIER
};

int main(int argc, char **argv)
{
    /* ---- Compile and cache the first code string ---- */

    TCCState *s1 = tcc_new();
    SIMPLE_SETUP(s1);
    tcc_save_extended_symtab(s1);
    if (tcc_compile_string(s1, struct_code) == -1) return 1;

    extended_symtab_p my_symtab = tcc_get_extended_symbol_table(s1);
    if (!tcc_serialize_extended_symtab(my_symtab, "struct.cache")) return 1;

    tcc_delete(s1);
    s1 = NULL;
    tcc_delete_extended_symbol_table(my_symtab);
    my_symtab = NULL;
    pass("Struct definition code string compiled and cached fine");

    /* ---- Load the symtab from the cached file ---- */

    my_symtab = tcc_deserialize_extended_symtab("struct.cache");
    ok(my_symtab != NULL, "Deserialization returned non-null symtab pointer");
    ok(tcc_extended_symtab_test(my_symtab, TS_TEST_HAS_STRUCT, "point"),
        "token 'point' has struct symbol");

    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Check code string that depends on the struct definition ---- */

    TCCState *s_consume = tcc_new();
    setup_and_relocate_second_state(s_consume, consuming_code);
    int (*tester_ptr)() = tcc_get_symbol(s_consume, "tester");
    if (tester_ptr == NULL) goto FAIL;
    is_i(tester_ptr(), 34, "tester code returns correct value");

    /* ---- clean up the memory ---- */

    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s_consume);
    remove("struct.cache");
    pass("cleanup");

    done_testing();
    return 0;

FAIL:
    remove("struct.cache");
    return 1;
}
