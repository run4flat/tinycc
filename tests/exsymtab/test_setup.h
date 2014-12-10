/* Common compiler state setup code */

#include "tap.h"
#include "libtcc.h"
#include <stdlib.h>

#ifndef DIAG
  #define DIAG(message, extra)
#endif

#ifdef INCLUDE_MALLOC
  #define APPLY_MALLOC(state)                  \
      tcc_add_symbol(state, "malloc", malloc); \
	  tcc_add_symbol(state, "free", free);
#else
  #define APPLY_MALLOC(state)
#endif

/******** Setup first compiler state, with a symbol table ********/

void copy_symtab(extended_symtab_p copied_symtab, void * data) {
	extended_symtab_p* my_symtab_p = (extended_symtab_p*)data;
	*my_symtab_p = copied_symtab;
}

#define SIMPLE_SETUP(state)                           \
	if (!state) return 1;                             \
	if (argc == 2 && !memcmp(argv[1], "lib_path=",9)) \
		tcc_set_lib_path(state, argv[1]+9);           \
	else                                              \
		tcc_set_lib_path(state, "../..");             \
	tcc_set_output_type(state, TCC_OUTPUT_MEMORY);

#define setup_and_compile_s1(symtab, code)                                    \
    SIMPLE_SETUP(s1);                                                         \
	tcc_set_extended_symtab_callbacks(s1, &copy_symtab, NULL, NULL, &symtab); \
    if (tcc_compile_string(s1, code) == -1) return 1;                         \
	APPLY_MALLOC(s1);                                                         \
	if (tcc_relocate(s1, TCC_RELOCATE_AUTO) == -1) return 1;                  \
	pass("First code string compiled and relocated fine")

/******** Setup the ensuing compiler states with symbol tables ********/

typedef struct {
	TCCState * second_context;
	TCCState * first_context;
	extended_symtab_p first_symtab;
} second_callback_data;

TokenSym_p lookup_by_name (char * name, int len, void * data,
	extended_symtab_p*containing_symtab
) {
	/* Extract the name from the full string passed in */
	char name_to_find[len + 1];
	strncpy(name_to_find, name, len);
	name_to_find[len] = '\0';

	/* Pull out the symtab */
	extended_symtab_p my_symtab = ((second_callback_data*)data)->first_symtab;
	*containing_symtab = my_symtab;
	/* Get the tokensym and return if found */
	TokenSym_p ts = tcc_get_extended_tokensym(my_symtab, name_to_find);
	if (ts != NULL) return ts;
	/* Warn otherwise */
	DIAG("Did not find [%s]", name_to_find);
	return NULL;
}

void sym_used (char * name, int len, void * data) {
	/* Extract the name from the full string passed in */
	DIAG("Adding external identifier %s to second context\n", name);
	
	/* Unpack the two compilation contexts */
	second_callback_data * my_data = (second_callback_data *)data;
	TCCState * curr_context = my_data->second_context;
	TCCState * orig_context = my_data->first_context;
	
	/* Get the symbol and add it */
	void * orig_symbol = tcc_get_symbol(orig_context, name);
	if (!orig_symbol) {
		DIAG("COULD NOT FIND %s!!\n", name);
		return;
	}
	tcc_add_symbol(curr_context, name, orig_symbol);
}

/* ---- code for setting up the second compiler state ---- */

#define SETUP_SECOND_CALLBACK_DATA(state)   \
	second_callback_data callback_data;     \
	callback_data.first_symtab = my_symtab; \
	callback_data.first_context = state

#define setup_and_compile_second_state(s, code)                 \
	if (!s) return 1;                                           \
	if (argc == 2 && !memcmp(argv[1], "lib_path=",9))           \
		tcc_set_lib_path(s, argv[1]+9);                         \
	else                                                        \
	    tcc_set_lib_path(s, "../..");                           \
	tcc_set_output_type(s, TCC_OUTPUT_MEMORY);                  \
	callback_data.second_context = s;                           \
	tcc_set_extended_symtab_callbacks(s, NULL, &lookup_by_name, \
		&sym_used, &callback_data);                     \
   	if (tcc_compile_string(s, code) == -1) return 1

#define relocate_second_state(s)                                \
	if (tcc_relocate(s, TCC_RELOCATE_AUTO) == -1) return 1;     \
	pass("Dependent code string compiled and relocated fine")

#define setup_and_relocate_second_state(s, code)                \
	setup_and_compile_second_state(s, code);                    \
	relocate_second_state(s)
