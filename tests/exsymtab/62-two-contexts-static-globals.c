/*
 * Declare a global variable in one context and access it in another context.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"
#include <string.h>

/* Create a custom error handler that merely tracks that the correct
 * error was thrown. */
int error_thrown;
void my_error_func (void * data, const char * msg ) {
    if (strstr(msg, "undefined symbol 'test_var'")) error_thrown++;
}

char first_code[] =
"static unsigned int test_var = 42;\n"
"unsigned int get_test_var () {\n"
"    return test_var;\n"
"}\n"
;

char second_code[] =
"unsigned int indirect_get() {\n"
"    return get_test_var();\n"
"}\n"
"unsigned int direct_get() {\n"
"    return test_var;\n"
"}\n"
;

int main(int argc, char **argv)
{
    /* ---- Compile the first code string and setup the callback data ---- */

    TCCState *s1 = tcc_new();
    extended_symtab_p my_symtab;
    setup_and_compile_s1(my_symtab, first_code);
    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Make sure the getter and setter actually modify global state ---- */
    unsigned int (*first_get)() = tcc_get_symbol(s1, "get_test_var");
    is_i(first_get(), 42, "first_set/first_get work");

    /* ---- Check for errors during dependent compilation ---- */

    TCCState *s2 = tcc_new();
    if (!s2) return 1;
    if (argc == 2 && !memcmp(argv[1], "lib_path=",9))
        tcc_set_lib_path(s2, argv[1]+9);
    else
        tcc_set_lib_path(s2, "../..");

    tcc_set_output_type(s2, TCC_OUTPUT_MEMORY);
    tcc_set_error_func(s2, my_error_func, my_error_func);
    callback_data.second_context = s2;
    tcc_set_extended_symtab_callbacks(s2, &lookup_by_name, &sym_used, &prep_table, &callback_data);
    tcc_compile_string(s2, second_code);
    tcc_relocate(s2, TCC_RELOCATE_AUTO);

    /* See if the error was thrown */
    is_i(error_thrown, 1, "Static global variables do not bleed scope");

    /* ---- clean up the memory ---- */

    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s1);
    tcc_delete(s2);
    pass("cleanup");

    done_testing();
    return 0;
}
