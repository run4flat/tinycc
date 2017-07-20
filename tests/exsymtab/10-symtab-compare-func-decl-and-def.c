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

void compare_two_syms (exsymtabSym * decl, exsymtabSym * def, int indentation) {
    #define compare(field, descr) is_i(decl-> field, def-> field, "    " descr " agree" + 4 - indentation)
    /* Basic members of the Sym */
    compare(r, "associated registers");
    compare(c, "associated numbers/Elf symbol indexes");
    compare(type.t, "type.t values");
    
    /* members of SymAttr */
    compare(a.aligned, "SymAttr allignment");
    compare(a.packed, "SymAttr packing");
    compare(a.weak, "SymAttr weak");
    compare(a.visibility, "SymAttr visibility");
    compare(a.dllexport, "SymAttr dllexport");
    compare(a.dllimport, "SymAttr dllimport");
	
	/* members of FuncAttr */
    compare(f.func_call, "FuncAttr func_call");
    compare(f.func_type, "FuncAttr func_type");
    if (indentation == 1) {
		ok(def->f.func_body != decl->f.func_body,
			"    FuncAttr func_body do NOT AGREE, as expected" + 4 - indentation);
	}
	else compare(f.func_body, "FuncAttr func_body");
    compare(f.func_args, "FuncAttr func_args");
	
	/* next field */
	if (decl->next != NULL && def->next != NULL) {
		printf("%s", "    Comparing next fields...\n" + 4 - indentation);
		compare_two_syms (decl->next, def->next, indentation + 1);
	}
	else if (decl->next != NULL || def->next != NULL) {
		fail("    next fields are not both null (or both non-null)" + 4 - indentation);
	}
	
	/* type.ref field */
	if (decl->type.ref != NULL && def->type.ref != NULL) {
		printf("%s", "    Comparing type.ref fields...\n" + 4 - indentation);
		compare_two_syms (decl->type.ref, def->type.ref, indentation + 1);
	}
	else if (decl->type.ref != NULL || def->type.ref != NULL) {
		fail("    type.ref fields are not both null (or both non-null)" + 4 - indentation);
	}
}

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
    exsymtabTokenSym * ts = tcc_get_extended_tokensym(decl_symtab, "foo");
    if (ts == NULL) {
        printf("could not find foo tokensym in declaration symtab\n");
        return(1);
    }
    if (ts->sym_identifier == NULL) {
        printf("foo TokenSym in declaration has no sym_identifier\n");
        return(1);
    }

    exsymtabSym * foo_decl = ts->sym_identifier;

    ts = tcc_get_extended_tokensym(def_symtab, "foo");
    if (ts == NULL) {
        printf("could not find foo tokensym in definition symtab\n");
        return(1);
    }
    if (ts->sym_identifier == NULL) {
        printf("foo TokenSym in definition has no sym_identifier\n");
        return(1);
    }

    exsymtabSym * foo_def = ts->sym_identifier;

    /* ---- Compare them ---- */
    compare_two_syms(foo_decl, foo_def, 0);

    /* Clean up */
    tcc_delete_extended_symbol_table(def_symtab);
    tcc_delete_extended_symbol_table(decl_symtab);

    return done_testing();
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
