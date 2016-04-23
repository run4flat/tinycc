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

void compare_funcs(Sym * foo_decl, Sym * foo_def) {
	is_i(foo_def->r, foo_decl->r, "foo->r agree");
	is_i(foo_def->c, foo_decl->c, "foo->c agree");
	is_i(foo_def->type.t, foo_decl->type.t, "foo->type.t agree");
	
	Sym * def_ret = foo_def->type.ref;
	Sym * dec_ret = foo_decl->type.ref;
	/* This is the only difference. The value 0x10000 indicates "This is just a
	 * prototype," which means a later definition is allowed. I do not want a
	 * later redefinition for functions that have been defined in an earlier
	 * context, so these are allowed (and encouraged) to differ. */
	is_i(dec_ret->r, 0x11000, "declaration ret->r is 0x11000");
	is_i(def_ret->r, 0x1000,  "definition  ret->r is 0x01000");
	is_i(def_ret->c, dec_ret->c, "ret->r agree");
	is_i(def_ret->type.t, dec_ret->type.t, "ret->type.t agree");
	
	Sym * def_arg1 = def_ret->next;
	Sym * dec_arg1 = dec_ret->next;
	is_i(def_arg1->r, dec_arg1->r, "arg1->r agree");
	is_i(def_arg1->c, dec_arg1->c, "arg1->c agree");
	is_i(def_arg1->type.t, dec_arg1->type.t, "arg1->type.t agree");
	
	Sym * def_arg2 = def_arg1->next;
	Sym * dec_arg2 = dec_arg1->next;
	is_i(def_arg2->r, dec_arg2->r, "arg2->r agree");
	is_i(def_arg2->c, dec_arg2->c, "arg2->c agree");
	is_i(def_arg2->type.t, dec_arg2->type.t, "arg2->type.t agree");
	
	Sym * def_arg2pt = def_arg2->type.ref;
	Sym * dec_arg2pt = dec_arg2->type.ref;
	is_i(def_arg2pt->r, dec_arg2pt->r, "arg2pt->r agree");
	is_i(def_arg2pt->c, dec_arg2pt->c, "arg2pt->c agree");
	is_i(def_arg2pt->type.t, dec_arg2pt->type.t, "arg2pt->type.t agree");
}

int main(int argc, char **argv) {
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
    
    diag("Before dependent compilation");
    compare_funcs(foo_decl, foo_def);
	
	/* compile the dependent code block */
	TCCState *s_consumer = tcc_new();
	second_callback_data callback_data;
	callback_data.first_symtab = def_symtab;
	setup_and_compile_second_state(s_consumer, consumer_code);
    
    diag("After dependent compilation, before symbol addition");
    compare_funcs(foo_decl, foo_def);
	
	tcc_add_symbol(s_consumer, "foo", foo_ptr);
    
    diag("After symbol addition, before relocation & free");
    compare_funcs(foo_decl, foo_def);
	
	relocate_second_state(s_consumer);
	tcc_free(s_consumer);
	
	/* ---- Compare them ---- */
    diag("After dependent compilation");
    compare_funcs(foo_decl, foo_def);
	
	/* Clean up */
	tcc_delete_extended_symbol_table(def_symtab);
	tcc_delete_extended_symbol_table(decl_symtab);
	
	return done_testing();
}
