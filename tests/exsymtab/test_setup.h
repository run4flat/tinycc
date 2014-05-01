/* Common compiler state setup code */

#include "tap.h"
#include "libtcc.h"
#include <stdlib.h>

#ifndef DIAG
  #define DIAG
#endif

/******** Setup first compiler state, with a symbol table ********/

void copy_symtab(TokenSym_p* copied_symtab, void * data) {
	TokenSym_p** my_symtab_p = (TokenSym_p**)data;
	*my_symtab_p = copied_symtab;
}

#define setup_and_compile_s1(symtab, code)                                    \
	if (!s1) return 1;                                                        \
	if (argc == 2 && !memcmp(argv[1], "lib_path=",9))                         \
		tcc_set_lib_path(s1, argv[1]+9);                                      \
	else                                                                      \
		tcc_set_lib_path(s1, "../..");                                        \
	tcc_set_output_type(s1, TCC_OUTPUT_MEMORY);                               \
	tcc_set_extended_symtab_callbacks(s1, &copy_symtab, NULL, NULL, &symtab); \
    if (tcc_compile_string(s1, first_code) == -1) return 1;                   \
	if (tcc_relocate(s1, TCC_RELOCATE_AUTO) == -1) return 1;                  \
	pass("First code string compiled and relocated fine")

/******** Setup the ensuing compiler states with symbol tables ********/

typedef struct {
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
		if (strncmp(name, tcc_tokensym_name(my_symtab[i]), len) == 0) {
			DIAG("Found [%s]", name_to_find);
			return my_symtab[i];
		}
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

	DIAG("About to return tokensym pointer %p", to_return);
	/* All done; return the symbol */
	return to_return;
}

/* ---- code for setting up the second compiler state ---- */

#define SETUP_SECOND_CALLBACK_DATA      \
	second_callback_data callback_data; \
	callback_data.first_symtab = my_symtab

#define setup_and_compile_second_state(s, code)                 \
	if (!s) return 1;                                           \
	if (argc == 2 && !memcmp(argv[1], "lib_path=",9))           \
		tcc_set_lib_path(s, argv[1]+9);                         \
	else                                                        \
	    tcc_set_lib_path(s, "../..");                           \
	tcc_set_output_type(s, TCC_OUTPUT_MEMORY);                  \
	callback_data.second_context = s;                           \
	tcc_set_extended_symtab_callbacks(s, NULL, &lookup_by_name, \
		&lookup_by_number, &callback_data);                     \
   	if (tcc_compile_string(s, code) == -1) return 1

#define relocate_second_state(s)                                \
	if (tcc_relocate(s, TCC_RELOCATE_AUTO) == -1) return 1;     \
	pass("Dependent code string compiled and relocated fine")

#define setup_and_relocate_second_state(s, code)                \
	setup_and_compile_second_state(s, code);                    \
	relocate_second_state(s)
