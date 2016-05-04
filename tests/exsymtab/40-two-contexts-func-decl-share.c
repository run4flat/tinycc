/*
 * Share function declarations between two contexts.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char first_code[] =
"int get_const();\n"
;

char second_code[] =
"int mult_const(int factor) {\n"
"    return factor * get_const();"
"\n}"
;

int my_const;
int supply_const() { return my_const; }

int main(int argc, char **argv)
{
    my_const = 60;

    /* ---- Compile the first code string and setup the callback data ---- */

    TCCState *s1 = tcc_new();
    extended_symtab_p my_symtab;
    setup_and_compile_s1(my_symtab, first_code);
    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Compile the second string ---- */

    TCCState *s2 = tcc_new();
    setup_and_compile_second_state(s2, second_code);
    tcc_add_symbol(s2, "get_const", supply_const);
    relocate_second_state(s2);

    /* ---- Check the function pointer addresses ---- */

    /* Is fib in the correct location? */
    int (*mult_const)(int) = tcc_get_symbol(s2, "mult_const");
    if (mult_const == NULL) return -1;
    pass("Found mult_const function pointer");

    /* ---- Make sure the function invocation gives the right answer ---- */

    is_i(mult_const(10), my_const*10, "mult_const function call works");
    my_const = -4;
    is_i(mult_const(4), my_const * 4, "mult_const function call works again");

    /* ---- Cleanup ---- */
    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s1);
    tcc_delete(s2);
    pass("cleanup");

    return done_testing();
}
