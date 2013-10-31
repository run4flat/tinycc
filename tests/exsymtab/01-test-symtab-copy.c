/*
 * Simple library to output tests using the Test Anything Protocol.
 * This is far from being a complete TAP producer. For that, see libtap.
 * For a discussion of the TAP output format, see
 * http://podwiki.hexten.net/TAP/TAP.html?page=TAP
 *
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

void copy_symtab(TokenSym ** copied_symtab, void * data) {
	TokenSym*** my_symtab_p = (TokenSym***)data;
	*my_symtab_p = copied_symtab;
}

int main(int argc, char **argv) {
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
	
	/* Set the copy callback */
	TokenSym ** my_symtab;
	tcc_set_extended_symtab_callbacks(s, &copy_symtab, NULL, NULL, &my_symtab);
	pass("Set the symtab copy function");

    if (tcc_compile_string(s, to_compile) == -1)
        return 1;
	pass("Compiled the test code");
	
	/* The token symbol table will include tokens that are not attached
	 * to symbols. The globally accessible stuff must have an affiliated
	 * symbol of some sort. We can count the number of global symbols
	 * and confirm that it is what we expect: fib, linked_list, and PI
	 */
	int i, N_symbols = 0;
	int fib_is_identifier = 0, PI_is_define = 0, linked_list_is_struct = 0;
	for (i = 0; i < tcc_tokensym_list_length(my_symtab); i++) {
		if (tcc_tokensym_has_define(my_symtab[i])) {
			if (strcmp("PI", tcc_tokensym_name(my_symtab[i])) == 0)
				PI_is_define = 1;
			N_symbols++;
		}
		if (tcc_tokensym_has_identifier(my_symtab[i])) {
			if (strcmp("fib", tcc_tokensym_name(my_symtab[i])) == 0)
				fib_is_identifier = 1;
			N_symbols++;
		}
		if (tcc_tokensym_has_struct(my_symtab[i])) {
			if (strcmp("linked_list", tcc_tokensym_name(my_symtab[i])) == 0)
				linked_list_is_struct = 1;
			N_symbols++;
		}
	}
	
	/* Can we find the fib symbol? */
	is_i(N_symbols, 3, "Reported symbol table length is correct");
	ok(fib_is_identifier, "fib is an identifier");
	ok(PI_is_define, "PI is a macro");
	ok(linked_list_is_struct, "linked_list is a struct");
	
	/* Clean up */
	tcc_delete_extended_symbol_table(my_symtab);
	pass("Cleaned up extended symbol table memory");
	
	done_testing();
	
	return 0;
}
