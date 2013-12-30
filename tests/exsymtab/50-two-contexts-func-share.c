/*
 * Share functions and macros between two contexts without having to
 * recompile anything, not even the function declarations or macro
 * definitions.
 */

#include "tap.h"
#include "libtcc.h"
#include <stdlib.h>

/* Set to true to enable diagnostic output */
#if 0
	#define DIAG(...) diag(__VA_ARGS__)
#else
	#define DIAG(...)
#endif

char first_code[] =
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

char second_code[] =
"int fib_of_5() {\n"
"    return fib(5);\n"
"}\n"
"\n"
"double area_of_circle(double radius) {\n"
"    return PI * radius * radius;\n"
"}\n"
;

void copy_symtab(TokenSym_p* copied_symtab, void * data) {
	TokenSym_p** my_symtab_p = (TokenSym_p**)data;
	*my_symtab_p = copied_symtab;
}

typedef struct {
	TCCState * first_context;
	TCCState * second_context;
	TokenSym_p* first_symtab;
} second_callback_data;

TokenSym_p lookup_by_name (char * name, int len, void * data, int is_identifier) {
	/* Extract the name from the full string passed in */
	char name_to_find[len + 1];
	strncpy(name_to_find, name, len);
	name_to_find[len] = '\0';
	/* Pull out the symtab */
	TokenSym_p* my_symtab = ((second_callback_data*)data)->first_symtab;
	int i;
	for (i = 0; i < tcc_tokensym_list_length(my_symtab); i++) {
		if (
			tcc_tokensym_is_shareable(my_symtab[i])
			&& strncmp(name, tcc_tokensym_name(my_symtab[i]), len) == 0
		) {
			DIAG("Found [%s]", name_to_find);
			return my_symtab[i];
		}
		DIAG("[%s] either does not look like [%s] or is not shareable"
			, tcc_tokensym_name(my_symtab[i]), name_to_find);
	}
	DIAG("Did not find [%s]", name_to_find);
	return NULL;
}

TokenSym_p lookup_by_number (int tok_id, void * data, int is_identifier) {
	/* Unpack the data */
	DIAG("Looking up tokensym for %X", tok_id);
	second_callback_data * my_data = (second_callback_data *)data;
	/* Is this token in our extended symtab? */
	TokenSym_p* my_symtab = my_data->first_symtab;
	DIAG("First tokensym is for %X", tcc_tokensym_tok(my_symtab[0]));
	if (!tcc_token_is_in_extended_symtab(tok_id, my_symtab)) return NULL;
	/* If so, find it */
	DIAG("It appears that %X is in our extended symtab", tok_id);
	int first_tok = tcc_tokensym_no_extra_bits(tcc_tokensym_tok(my_symtab[0]));
	tok_id = tcc_tokensym_no_extra_bits(tok_id);
	TokenSym_p to_return = my_symtab[tok_id - first_tok];

#if 0	
	/* If this is an identifier request, we should also add the pointer
	 * to the compiled object code for this identifier. */
	if (is_identifier) {
		/* Get the symbol by this name */
diag("Retrieving symbol pointer for [%s]", tcc_tokensym_name(to_return));
		void * symbol_to_add = tcc_get_symbol(my_data->first_context,
			tcc_tokensym_name(to_return));
		tcc_add_symbol(my_data->second_context,
			tcc_tokensym_name(to_return), symbol_to_add);
	}
#endif
	
	DIAG("About to return tokensym pointer %p", to_return);
	/* All done; return the symbol */
	return to_return;
}

int main(int argc, char **argv) {
    TCCState *s1 = tcc_new();
    if (!s1) return 1;
	pass("Allocated first tcc state");
    
    /* if tcclib.h and libtcc1.a are not installed, where can we find them */
    if (argc == 2 && !memcmp(argv[1], "lib_path=",9))
        tcc_set_lib_path(s1, argv[1]+9);
	
    /* MUST BE CALLED before any compilation */
    tcc_set_output_type(s1, TCC_OUTPUT_MEMORY);
	
	/* Set the copy callback */
	TokenSym_p* my_symtab;
	tcc_set_extended_symtab_callbacks(s1, &copy_symtab, NULL, NULL, &my_symtab);
	pass("Set the symtab copy function");

    if (tcc_compile_string(s1, first_code) == -1) return 1;
	pass("Compiled the first code string");
	
	if (tcc_relocate(s1, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Relocated the first code string");
	
	/* Get the Fibonaci function and evaluate it */
	int (*fib_from_first)(int) = tcc_get_symbol(s1, "fib");
	if (fib_from_first == NULL) return -1;
	pass("Found fib");
	is_i(fib_from_first(5), 5, "Calling fib from first compiler context works");
	
	/* Create a second context and add the extended symbol table to it */
	TCCState *s2 = tcc_new();
	if (!s2) return 1;
	pass("Built a second compiler state");
    /* if tcclib.h and libtcc1.a are not installed, where can we find them */
    if (argc == 2 && !memcmp(argv[1], "lib_path=",9))
        tcc_set_lib_path(s2, argv[1]+9);
	
	tcc_set_output_type(s2, TCC_OUTPUT_MEMORY);
	pass("Set the output to in-memory");
	
	/* Set the extended symbol table callbacks */
	second_callback_data callback_data;
	callback_data.first_context = s1;
	callback_data.second_context = s2;
	callback_data.first_symtab = my_symtab;
	tcc_set_extended_symtab_callbacks(s2, NULL, &lookup_by_name,
		&lookup_by_number, &callback_data);
	pass("Set the symtab callbacks for the second compiler state");
	
    if (tcc_compile_string(s2, second_code) == -1) return 1;
	pass("Compiled the second code string");
	tcc_add_symbol(s2, "fib", fib_from_first);
	
	if (tcc_relocate(s2, TCC_RELOCATE_AUTO) == -1) return 1;
	pass("Relocated the second code string");
	
	int (*fib_of_5_ptr)() = tcc_get_symbol(s2, "fib_of_5");
	if (fib_of_5_ptr == NULL) return -1;
	pass("Found fib_of_5 function pointer");
	
	/*is_i(fib_of_5_ptr(), 5, "Fibonaci function call works");*/
	
	double (*area_func)(double) = tcc_get_symbol(s2, "area_of_circle");
	if (area_func == NULL) return -1;
	double area = area_func(1);
	ok(3.1 < area && area < 3.2, "area function call works");
	
	/* finally, clean up the memory */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s2);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
