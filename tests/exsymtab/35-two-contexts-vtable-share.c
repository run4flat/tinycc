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
"struct point {\n"
"    int (*squared_distance)(struct point * self);\n"
"    int x;\n"
"    int y;\n"
"};\n"
"int point_squared_distance(struct point * self) {\n"
"    return self->x * self->x + self->y * self->y;\n"
"}\n"
"void * new_point(int x, int y) {\n"
"    struct point * pt = malloc(sizeof(struct point));\n"
"    pt->squared_distance = point_squared_distance;\n"
"    pt->x = x;\n"
"    pt->y = y;\n"
"    return pt;\n"
"}\n"
;

char second_code[] =
"void * distance_func_ptr(struct point * pt) {\n"
"    return pt->squared_distance;\n"
//" return pt->x;\n"
"}\n"
"int sq_distance_to_pt(struct point * pt) {\n"
"    return pt->squared_distance(pt);\n"
//"    return pt->x;\n"
"}\n"
;

int main(int argc, char **argv)
{
    /* ---- Compile the first code string and setup the callback data ---- */

    TCCState *s1 = tcc_new();
    extended_symtab_p my_symtab;
    setup_and_compile_s1(my_symtab, first_code);
    SETUP_SECOND_CALLBACK_DATA();

    /* ---- Allocate a point ---- */

    void* (*allocate_ptr)(int, int) = tcc_get_symbol(s1, "new_point");
    if (allocate_ptr == NULL) return 1;

    void * point_p = allocate_ptr(6, 9);
    if (point_p == NULL) {
        fail("Unable to allocate point");
        return 1;
    }
    else
        pass("Allocated point");

    /* ---- Check function address ---- */

    int (*squared_distance_func)(void*) = tcc_get_symbol(s1, "point_squared_distance");
    void * func_p = ((void **)point_p)[0];
    is_p(func_p, squared_distance_func, "Symbol table's function points to relocated location");

    /* ---- Make sure the function works correctly ---- */
    is_i(squared_distance_func(point_p), 9*9+6*6, "Vtable function works correctly");

    /* ---- Compile the second compiler context ---- */

    if (func_p == squared_distance_func) {
        TCCState * s_second = tcc_new();
        setup_and_relocate_second_state(s_second, second_code);

        /* Test pointer extraction method */
        void* (*second_extraction_func)(void*) = tcc_get_symbol(s_second, "distance_func_ptr");
        if (second_extraction_func == NULL) return 1;
        is_p(second_extraction_func(point_p), squared_distance_func,
            "address in vtable extracted in second context is correct");

        int (*sq_dist_ptr)(void*) = tcc_get_symbol(s_second, "sq_distance_to_pt");
        if (sq_dist_ptr == NULL) return 1;

        is_i(sq_dist_ptr(point_p), 9*9+6*6, "Second context able to call function pointer from struct");
        tcc_delete(s_second);
    }

    /* ---- clean up the memory ---- */

    tcc_delete_extended_symbol_table(my_symtab);
    tcc_delete(s1);
    free(point_p);
    pass("cleanup");

    return done_testing();
}
