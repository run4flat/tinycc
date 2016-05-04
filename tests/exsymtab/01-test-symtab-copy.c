/*
 * Exercises and tests the basic symbol copy behavior of the extended
 * symbol table API.
 */

#include "tap.h"
#include "libtcc.h"
#include <stdlib.h>

char to_compile[] =
"int fib(int n)\n"
"{\n"
"    if (n <= 2)\n"
"        return 1;\n"
"    else\n"
"        return fib(n-1) + fib(n-2);\n"
"}\n"
"\n"
"struct linked_list {\n"
"    void * next;\n"
"};\n"
"\n"
"#define PI 3.14159\n"
;

enum {
    GET_TOK,
    HAS_DEFINE,
    HAS_STRUCT,
    HAS_IDENTIFIER
};

int main(int argc, char **argv)
{
    TCCState *s;

    s = tcc_new();
    if (!s) {
        fprintf(stderr, "Could not create tcc state\n");
        exit(1);
    }

    pass("Allocated tcc state");

    /* if tcclib.h and libtcc1.a are not installed, where can we find them */
    if (argc == 2 && !memcmp(argv[1], "lib_path=",9))
        tcc_set_lib_path(s, argv[1]+9);

    /* MUST BE CALLED before any compilation */
    tcc_set_output_type(s, TCC_OUTPUT_MEMORY);
    pass("Set output type to memory");

    /* Tell the compiler to save an extended symbol table */
    tcc_save_extended_symtab(s);

    /* Compile the symbol table */
    if (tcc_compile_string(s, to_compile) == -1)
        return 1;
    pass("Compiled the test code");

    /* retrieve the symbol table and clean up the compiler */
    extended_symtab_p my_symtab = tcc_get_extended_symbol_table(s);
    tcc_delete(s);
    pass("Got extended symtab, cleaned up compilation state");

    /* See if the known things are accessible */
    ok(tcc_extended_symtab_test(my_symtab, HAS_DEFINE, "PI"), "PI is a macro");
    ok(tcc_extended_symtab_test(my_symtab, HAS_IDENTIFIER, "fib"), "fib is an identifier");
    ok(tcc_extended_symtab_test(my_symtab, HAS_STRUCT, "linked_list"), "linked_list is a struct");

    /* Clean up */
    tcc_delete_extended_symbol_table(my_symtab);
    pass("Cleaned up extended symtab memory");

    return done_testing();
}
