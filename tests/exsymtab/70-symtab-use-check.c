/*
 * This builds a symtab for a simple declaration and a full definition. Test
 * number 10 verified that these agree, so the next step is to use the
 * share the definition symtab and see if the dependent compilation makes any
 * alterations.
 */

#include "test_setup.h"
#include "tcc.h"

char definition_code[] = "double foo(int bar, double *baz) { return 0; }\n";
char declaration_code[] = "double foo(int bar, double *baz);\n";
char consumer_code[] = "double mult_foo(double factor) { return factor * foo(0, 0); }";

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
    compare(a.unsigned_enum, "SymAttr unsigned_enum");
	
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

    /* indicate that we want an extended symbol table produced */
    tcc_save_extended_symtab(s_decl);

    /* Compile */
    if (tcc_compile_string(s_decl, declaration_code) == -1) return 1;

    /* Get the extended symbol table */
    extended_symtab_p decl_symtab = tcc_get_extended_symbol_table(s_decl);

    /* All done with that compiler, clean up */
    tcc_free(s_decl);

    pass("Built declaration compiler state's symbol table");

    TCCState *s1 = tcc_new();
    extended_symtab_p def_symtab;
    setup_and_compile_s1(def_symtab, definition_code);
    double (*foo_ptr)(double) = tcc_get_symbol(s1, "foo");

    /* All done with that compiler, clean up */
    tcc_free(s1);

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

    diag("Before dependent compilation");
    compare_two_syms(foo_decl, foo_def, 0);

    /* compile the dependent code block */
    TCCState *s_consumer = tcc_new();
    second_callback_data callback_data;
    callback_data.first_symtab = def_symtab;
    setup_and_compile_second_state(s_consumer, consumer_code);

    diag("After dependent compilation, before symbol addition");
    compare_two_syms(foo_decl, foo_def, 0);

    tcc_add_symbol(s_consumer, "foo", foo_ptr);

    diag("After symbol addition, before relocation & free");
    compare_two_syms(foo_decl, foo_def, 0);

    relocate_second_state(s_consumer);
    tcc_free(s_consumer);

    /* ---- Compare them ---- */
    diag("After dependent compilation");
    compare_two_syms(foo_decl, foo_def, 0);

    /* Clean up */
    tcc_delete_extended_symbol_table(def_symtab);
    tcc_delete_extended_symbol_table(decl_symtab);

    return done_testing();
}
