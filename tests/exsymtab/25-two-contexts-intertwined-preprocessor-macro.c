/*
 * Share preprocessor macros between two contexts. In particular, one macro
 * uses a token that is defined in the second context.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

// Consuming code must create the identifier called "var"
char first_code[] =
"#define add_var_to(val) (val + var)\n"
;

char second_code[] = "int second() {\n"
"       int var = 5;\n"
"       return add_var_to(4);\n"
"}\n";

int main(int argc, char **argv)
{
    /* ---- Compile the first code string and setup the callback data ---- */

    TCCState *s1 = tcc_new();
    extended_symtab_p my_symtab;
    setup_and_compile_s1(my_symtab, first_code);
    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Check code string that depends on the macro ---- */

    TCCState *s2 = tcc_new();
    setup_and_relocate_second_state(s2, second_code);
    int (*gives_nine)() = tcc_get_symbol(s2, "second");
    if (gives_nine == NULL) return 1;
    is_i(gives_nine(), 9, "First context's macro can refer to a token defined in second context");

    /* ---- clean up the memory ---- */

    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s1);
    tcc_delete(s2);
    pass("cleanup");

    return done_testing();
}
