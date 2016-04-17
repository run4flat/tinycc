/*
 * Declare a global variable in one context and access it in another context.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#include "test_setup.h"

char first_code[] =
"unsigned int test_var;\n"
"unsigned int get_test_var () {\n"
"    return test_var;\n"
"}\n"
"void set_test_var (unsigned int new_value) {\n"
"    test_var = new_value;\n"
"}\n"
;

char second_code[] =
"unsigned int indirect_get() {\n"
"    return get_test_var();\n"
"}\n"
"unsigned int direct_get() {\n"
"    return test_var;\n"
"}\n"
"void direct_set (unsigned int new_value) {\n"
"    test_var = new_value;\n"
"}\n"
"void indirect_set (unsigned int new_value) {\n"
"    set_test_var(new_value);\n"
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
    void (*first_set)(unsigned int) = tcc_get_symbol(s1, "set_test_var");
    first_set(42);
    is_i(first_get(), 42, "first_set/first_get work");
    first_set(439);
    is_i(first_get(), 439, "first_set/first_get really work");

    /* ---- Compile code string that depends on the function and global variable ---- */

    TCCState *s2 = tcc_new();
    setup_and_compile_second_state(s2, second_code);
    relocate_second_state(s2);

    /* ---- Check indirect getter and setter ---- */
    unsigned int (*indirect_get)() = tcc_get_symbol(s2, "indirect_get");
    void (*indirect_set)(unsigned int) = tcc_get_symbol(s2, "indirect_set");
    is_i(indirect_get(), 439, "indirect_get seems to work");
    indirect_set(276);
    is_i(indirect_get(), 276, "indirect set/get work together");
    is_i(first_get(), 276, "indirect set effects direct result of first_get");
    first_set(1024);
    is_i(indirect_get(), 1024, "first_set/indirect_get work together");

    /* ---- Check direct getter and setter ---- */
    unsigned int (*direct_get)() = tcc_get_symbol(s2, "direct_get");
    void (*direct_set)(unsigned int) = tcc_get_symbol(s2, "direct_set");
    is_i(direct_get(), 1024, "direct_get seems to work");
    direct_set(543);
    is_i(direct_get(), 543, "direct set/get work together");
    is_i(first_get(), 543, "direct set effects direct result of first_get");
    first_set(2781);
    is_i(direct_get(), 2781, "first_set/direct_get work together");

    /* ---- clean up the memory ---- */

    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s1);
    tcc_delete(s2);
    pass("cleanup");

    done_testing();
    return 0;
}
