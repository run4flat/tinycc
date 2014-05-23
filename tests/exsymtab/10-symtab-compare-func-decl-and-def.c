/*
 * Dump the symtab layout for a function declaration and a function
 * definition. This needs to be run at least once with a modified 
 * copy_extended_symtab in libtcc.c so that we have minimal modification
 * between the original and the copy. These results should be stored
 * and compared with the output when copy_extended_symtab is re-enabled
 * and fully functional.
 */

#include "libtcc.h"
#include "tcc.h"
#include "tap.h"
#include <stdlib.h>

char definition_code[] = "double foo(int bar, double *baz) { return 0; }\n";
char declaration_code[] = "double foo(int bar, double *baz);\n";

void copy_symtab(TokenSym_p* copied_symtab, void * data) {
	TokenSym_p** my_symtab_p = (TokenSym_p**)data;
	*my_symtab_p = copied_symtab;
}

int main(int argc, char **argv) {
    TCCState *s_decl = tcc_new();
    if (!s_decl) {
        fprintf(stderr, "Could not create tcc state\n");
        exit(1);
    }
    /* if tcclib.h and libtcc1.a are not installed, where can we find them */
    if (argc == 2 && !memcmp(argv[1], "lib_path=",9))
        tcc_set_lib_path(s_decl, argv[1]+9);
    /* MUST BE CALLED before any compilation */
    tcc_set_output_type(s_decl, TCC_OUTPUT_MEMORY);
	/* Set the copy callback */
	TokenSym_p* decl_symtab;
	tcc_set_extended_symtab_callbacks(s_decl, &copy_symtab, NULL, NULL, &decl_symtab);
	/* Compile */
    if (tcc_compile_string(s_decl, declaration_code) == -1) return 1;
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
	/* Set the copy callback */
	TokenSym_p* def_symtab;
	tcc_set_extended_symtab_callbacks(s_def, &copy_symtab, NULL, NULL, &def_symtab);
	/* Compile */
    if (tcc_compile_string(s_def, definition_code) == -1) return 1;
    /* All done with that compiler, clean up */
    tcc_free(s_def);

    pass("Built definition compiler state's symbol table");
    
    /* get the symbol table layouts of the two */
	int i;
	Sym * foo_decl;
	for (i = 0; i < tcc_tokensym_list_length(decl_symtab); i++) {
		if (strcmp("foo", tcc_tokensym_name(decl_symtab[i])) == 0) {
			if (decl_symtab[i]->sym_identifier == NULL) {
				printf("foo TokenSym has no sym_identifier???\n");
				return(1);
			}
			foo_decl = decl_symtab[i]->sym_identifier;
		}
	}
    Sym * foo_def;
    for (i = 0; i < tcc_tokensym_list_length(def_symtab); i++) {
    	if (strcmp("foo", tcc_tokensym_name(def_symtab[i])) == 0) {
    		if (def_symtab[i]->sym_identifier == NULL) {
	    		printf("foo TokenSym has no sym_identifier???\n");
	    		return(1);
    		}
    		foo_def = def_symtab[i]->sym_identifier;
    	}
    }
	
	/* ---- Compare them ---- */
	
	is_i(foo_decl->r, foo_def->r, "foo->r agree");
	is_i(foo_decl->c, foo_def->c, "foo->c agree");
	is_i(foo_decl->type.t, foo_def->type.t, "foo->type.t agree");
	
	Sym * def_ret = foo_def->type.ref;
	Sym * dec_ret = foo_decl->type.ref;
	is_i(dec_ret->r, 0x11000, "declaration return r field is 0x11000");
	is_i(def_ret->r, 0x1000, "definition return r field is 0x1000");
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