/*
 * See how well caching works for preprocessor macros.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char def_code[] =
"#define one 1\n"
"#define MAX(first, second) (first < second ? second : first)\n"
"#define swap_int(first, second) do { \\\n"
"    int temp; \\\n"
"    temp = first; \\\n"
"    first = second; \\\n"
"    second = temp; \\\n"
"  } while(0)\n"
"\n"
;

char second_one_code[] = "int second_one() { return one; }\n";
char second_max_code[] =
"int second_max(int l, int r) { return MAX(l, r); }\n";
char second_swap_code[] =
"int second_swap(int l, int r) { swap_int(l, r); return l; } \n";

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
    /* define something "from the command line." This is meant to
     * trigger disagreement between the "universal" portions of the
     * cached and local symbol tables. */
    tcc_define_symbol(s1, "foo", "bar");
    /* setup */
    SIMPLE_SETUP(s1);
    tcc_save_extended_symtab(s1);
    if (tcc_compile_string(s1, def_code) == -1) return 1;

    extended_symtab_p my_symtab = tcc_get_extended_symbol_table(s1);
    if (!tcc_serialize_extended_symtab(my_symtab, "defines.cache")) return 1;

    tcc_delete(s1);
    s1 = NULL;
    tcc_delete_extended_symbol_table(my_symtab);
    my_symtab = NULL;
    pass("Define code string compiled and cached fine");

    /* ---- Load the symtab from the cached file ---- */

    my_symtab = tcc_deserialize_extended_symtab("defines.cache");
    ok(my_symtab != NULL, "Deserialization returned non-null symtab pointer");
    ok(tcc_extended_symtab_test(my_symtab, TS_TEST_HAS_DEFINE, "one"),
        "token 'one' has define symbol");
    ok(tcc_extended_symtab_test(my_symtab, TS_TEST_HAS_DEFINE, "MAX"),
        "token 'MAX' has define symbol");
    ok(tcc_extended_symtab_test(my_symtab, TS_TEST_HAS_DEFINE, "swap_int"),
        "token 'swap_int' has define symbol");

    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Check code string that depends on the 'one' macro ---- */

    TCCState *s_one = tcc_new();
    setup_and_relocate_second_state(s_one, second_one_code);
    int (*second_one_ptr)() = tcc_get_symbol(s_one, "second_one");
    if (second_one_ptr == NULL) goto FAIL;
    is_i(second_one_ptr(), 1, "second_one call works");

    /* ---- Check code string that depends on the 'MAX' macro ---- */

    TCCState *s_max = tcc_new();
    setup_and_relocate_second_state(s_max, second_max_code);
    int (*second_max_ptr)() = tcc_get_symbol(s_max, "second_max");
    if (second_max_ptr == NULL) goto FAIL;
    is_i(second_max_ptr(10, 8), 10, "second_max call works for max in first slot");
    is_i(second_max_ptr(120, 245), 245, "second_max call works for max in second slot");

    /* ---- Check code string that depends on the 'swap_int' macro ---- */

    TCCState *s_swap = tcc_new();
    setup_and_relocate_second_state(s_swap, second_swap_code);
    int (*second_swap_ptr)(int, int) = tcc_get_symbol(s_swap, "second_swap");
    if (second_swap_ptr == NULL) goto FAIL;
    is_i(second_swap_ptr(5, 3), 3, "first slot of 'swap_int' macro passes");
    is_i(second_swap_ptr(3, 5), 5, "second slot of 'swap_int' macro passes");

    /* ---- clean up the memory ---- */

    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s_one);
    tcc_delete(s_max);
    tcc_delete(s_swap);
    remove("defines.cache");
    pass("cleanup");

    return done_testing();

FAIL:
    remove("defines.cache");
    return 1;
}
