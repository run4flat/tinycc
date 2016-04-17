/*
 * Compares the symtab layout for a function declaration and (in a separately
 * compiled context) a function definition. There should be only one difference
 * between them.
 */

#include "libtcc.h"
#include "tcc.h"
#include "test_setup.h"
#include <stdlib.h>

char definition_code[] = "double foo(int bar, double *baz) { return 0; }\n";
char declaration_code[] = "double foo(int bar, double *baz);\n";

int main(int argc, char **argv)
{
    TCCState *s_decl = tcc_new();
    SIMPLE_SETUP(s_decl);

    /* Indicate that we want the symtab */
    tcc_save_extended_symtab(s_decl);

    /* Compile */
    if (tcc_compile_string(s_decl, declaration_code) == -1) return 1;

    /* Get symtab */
    extended_symtab_p decl_symtab = tcc_get_extended_symbol_table(s_decl);

    /* All done with that compiler, clean up */
    tcc_free(s_decl);

    pass("Built declaration compiler state's symbol table");

    TCCState *s_def = tcc_new();
    if (!s_def) {
        fprintf(stderr, "Could not create tcc state\n");
        exit(1);
    }

    /* if tcclib.h and libtcc1.a are not installed, where can we find them */
    if (argc == 2 && !memcmp(argv[1], "lib_path=",9))
        tcc_set_lib_path(s_def, argv[1]+9);

    /* MUST BE CALLED before any compilation */
    tcc_set_output_type(s_def, TCC_OUTPUT_MEMORY);

    /* indicate that we want the symtab */
    tcc_save_extended_symtab(s_def);

    /* Compile */
    if (tcc_compile_string(s_def, definition_code) == -1) return 1;

    /* Get the symtab */
    extended_symtab_p def_symtab = tcc_get_extended_symbol_table(s_def);

    /* All done with that compiler, clean up */
    tcc_free(s_def);

    pass("Built definition compiler state's symbol table");

    /* get the symbol table layouts of the two */
    TokenSym * ts = tcc_get_extended_tokensym(decl_symtab, "foo");
    if (ts == NULL) {
        printf("could not find foo tokensym in declaration symtab\n");
        return(1);
    }
    if (ts->sym_identifier == NULL) {
        printf("foo TokenSym in declaration has no sym_identifier\n");
        return(1);
    }

    Sym * foo_decl = ts->sym_identifier;

    ts = tcc_get_extended_tokensym(def_symtab, "foo");
    if (ts == NULL) {
        printf("could not find foo tokensym in definition symtab\n");
        return(1);
    }
    if (ts->sym_identifier == NULL) {
        printf("foo TokenSym in definition has no sym_identifier\n");
        return(1);
    }

    Sym * foo_def = ts->sym_identifier;

    /* ---- Compare them ---- */

    is_i(foo_decl->r, foo_def->r, "foo->r agree");
    is_i(foo_decl->c, foo_def->c, "foo->c agree");
    is_i(foo_decl->type.t, foo_def->type.t, "foo->type.t agree");

    Sym * def_ret = foo_def->type.ref;
    Sym * dec_ret = foo_decl->type.ref;

    /* This is the only difference. The value 0x10000 indicates "This is just a
     * prototype," which means a later definition is allowed. I do not want a
     * later redefinition for functions that have been defined in an earlier
     * context, so these are allowed (and encouraged) to differ.
     */
    is_i(dec_ret->r, 0x11000, "declaration ret->r is 0x11000");
    is_i(def_ret->r, 0x1000,  "definition  ret->r is 0x01000");
    is_i(dec_ret->c, def_ret->c, "ret->r agree");
    is_i(dec_ret->type.t, def_ret->type.t, "ret->type.t agree");

    Sym * def_arg1 = def_ret->next;
    Sym * dec_arg1 = dec_ret->next;
    is_i(dec_arg1->r, def_arg1->r, "arg1->r agree");
    is_i(dec_arg1->c, def_arg1->c, "arg1->c agree");
    is_i(dec_arg1->type.t, def_arg1->type.t, "arg1->type.t agree");

    Sym * def_arg2 = def_arg1->next;
    Sym * dec_arg2 = dec_arg1->next;
    is_i(dec_arg2->r, def_arg2->r, "arg2->r agree");
    is_i(dec_arg2->c, def_arg2->c, "arg2->c agree");
    is_i(dec_arg2->type.t, def_arg2->type.t, "arg2->type.t agree");

    Sym * def_arg2pt = def_arg2->type.ref;
    Sym * dec_arg2pt = dec_arg2->type.ref;
    is_i(dec_arg2pt->r, def_arg2pt->r, "arg2pt->r agree");
    is_i(dec_arg2pt->c, def_arg2pt->c, "arg2pt->c agree");
    is_i(dec_arg2pt->type.t, def_arg2pt->type.t, "arg2pt->type.t agree");

    /* Clean up */
    tcc_delete_extended_symbol_table(def_symtab);
    tcc_delete_extended_symbol_table(decl_symtab);

    done_testing();
    return 0;
}

/* Original info:

Layout of definition function
Members of struct for foo:
  r is 230
  c is 2
  type.t is 6
Members for return type, i.e. foo->type.ref:
  ret->r is 1000
  ret->c is 1
  ret->type.t is 9 (floating point double should be 9)
Members for first argument, i.e. foo->type.ref->next:
  arg1->r is 0
  arg1->c is 0
  arg1->type.t is  is 0 (integer should be 0)
Members for second argument, i.e. foo->type.ref->next->next:
  arg2->r is 0
  arg2->c is 0
  arg2->type.t is 4 (pointer should be 4)
Members for second argument's pointer type, i.e. foo->type.ref->next->next->type.ref:
  arg2pt->r is 0
  arg2pt->c is ffffffff
  arg2pt->type.t is 9 (floating point double should be 9)

Layout of declaration function
Members of struct for foo:
  r is 230
  c is 0
  type.t is 86
Members for return type, i.e. foo->type.ref:
  ret->r is 11000
  ret->c is 1
  ret->type.t is 9 (floating point double should be 9)
Members for first argument, i.e. foo->type.ref->next:
  arg1->r is 0
  arg1->c is 0
  arg1->type.t is  is 0 (integer should be 0)
Members for second argument, i.e. foo->type.ref->next->next:
  arg2->r is 0
  arg2->c is 0
  arg2->type.t is 4 (pointer should be 4)
Members for second argument's pointer type, i.e. foo->type.ref->next->next->type.ref:
  arg2pt->r is 0
  arg2pt->c is ffffffff
  arg2pt->type.t is 9 (floating point double should be 9)

The c and type.t fields need to be updated. The difference with ret->r shows
that the declaration is a prototype, but the definition is not (see the
func_proto bit field of the AttributeDef struct in tcc.h.) Since I want to
make sure that later redefinitions get called out, I will keep this discrepancy
between the two. Note that I might want to update the weak field of the
AttributeDef...

*/
