/*
 * Share struct definitions between two contexts.
 */

/* uncomment to enable diagnostic output */
//      #define DIAG(...) diag(__VA_ARGS__)

#define INCLUDE_MALLOC
#include "test_setup.h"

char first_code[] =
"void * malloc(int);\n"
"void free(void *);\n"
"struct list {\n"
"    int N;\n"
"    int data[1];\n"
"};\n"
"void * new_list(int N) {\n"
"    struct list * new_list = malloc(sizeof(struct list) + sizeof(int) * (N-1));\n"
"    new_list->N = N;\n"
"    return new_list;\n"
"}\n"
;

char second_code[] =
"void * newz_list(int N) {\n"
"    struct list * list = new_list(N);\n"
"    int i;\n"
"    for (i = 0; i < N; i++) {\n"
"        list->data[i] = 0;\n"
"    }\n"
"    return list;\n"
"}\n"
"int sum(struct list * list) {\n"
"    int i, sum;\n"
"    sum = 0;\n"
"    for (i = 0; i < list->N; i++) {\n"
"        sum += list->data[i];\n"
"    }\n"
"    return sum;\n"
"}\n"
;

int main(int argc, char **argv)
{
    /* ---- Compile the first code string and setup the callback data ---- */

    TCCState *s1 = tcc_new();
    extended_symtab_p my_symtab;
    setup_and_compile_s1(my_symtab, first_code);
    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Allocate a point and manually unpack it ---- */
    void* (*allocate_list)(int) = tcc_get_symbol(s1, "new_list");
    if (allocate_list == NULL) return 1;

    void * list_p = allocate_list(2);
    if (list_p == NULL) {
        fail("Unable to allocate list");
        return 1;
    }
    else
        pass("Allocated list");

    int * manual_unpack = list_p;
    is_i(manual_unpack[0], 2, "manually unpacked N");
    free(list_p);

    /* ---- Compile the second compiler context ---- */
    TCCState * s_second = tcc_new();
    setup_and_relocate_second_state(s_second, second_code);
    void* (*newz_list)(int) = tcc_get_symbol(s_second, "newz_list");
    if (newz_list == NULL) return 1;
    list_p = newz_list(3);
    if (list_p == NULL) {
        fail("Unable to allocate list");
        return 1;
    }
    else
        pass("Allocated list");

    manual_unpack = list_p;
    is_i(manual_unpack[0], 3, "manually unpacked N (again)");
    is_i(manual_unpack[1], 0, "manually unpacked first element");
    is_i(manual_unpack[2], 0, "manually unpacked second element");

    int (*summer)(void *) = tcc_get_symbol(s_second, "sum");
    if (summer == NULL) return 1;

    manual_unpack[2] = 5;
    int the_sum = summer(list_p);
    is_i(the_sum, 5, "sum function works");

    /* ---- clean up the memory ---- */
    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s1);
    tcc_delete(s_second);
    free(list_p);
    pass("cleanup");

    return done_testing();
}
