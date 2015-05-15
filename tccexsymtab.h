/*
 *  TCC - Tiny C Compiler
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "tcc.h"

/*****************************************************************************/
/*                              compressed trie                              */
/*****************************************************************************/

struct compressed_trie {
	unsigned long long filled_bits;
	struct compressed_trie * children[1];
};
typedef struct compressed_trie c_trie;

c_trie * c_trie_new();
void c_trie_free(c_trie * curr);
void * c_trie_get_data (c_trie * head, char * string);
void c_trie_add_data (c_trie * head, char * string, void * data);

unsigned char _c_trie_popcount (unsigned long long v);
unsigned char _c_trie_bit_offset_for_char (char c);
c_trie ** _c_trie_find_child (c_trie * current, char * string);
c_trie** _c_trie_add_one_more_slot (c_trie** curr_p, c_trie * to_add, char slot_offset);

/******************************************************************************/
/*                           extended symtab struct                           */
/******************************************************************************/

typedef struct extended_symtab {
	Sym * sym_list;
	Sym * sym_last;
	Sym * def_list;
	Sym * def_last;
	c_trie * trie;
	int tok_start;
	TokenSym ** tokenSym_last;
	TokenSym * tokenSym_list [1];
} extended_symtab;

/******************************************************************************/
/*                           compiled symbol lookup                           */
/******************************************************************************/

void dump_sym_names(TCCState *state);
void copy_extended_symbols_to_exsymtab(TCCState *state);
/* tcc_get_extended_symbol_table in libtcc.h */
/* tcc_get_extended_tokensym in libtcc.h */
/* tcc_get_extended_symbol in libtcc.h */
/* tcc_get_next_extended_symbol_name in libtcc.h */

/******************************************************************************/
/*                            extended symtab copy                            */
/******************************************************************************/

/* tcc_set_extended_symtab_callbacks is in libtcc.h */
/* tcc_save_extended_symtab is in libtcc.h */

int _sym_is_all_zeros(Sym * to_check);
Sym * _get_new_sym_or_def_pointer (Sym * old, Sym * new_list, int offset_of_last, Sym * stack);
Sym * get_new_symtab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last);
Sym * get_new_deftab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last);
int tokenstream_len (int * stream);
void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start);
LIBTCCAPI void tcc_delete_extended_symbol_table (extended_symtab * symtab);
LIBTCCAPI int tcc_extended_symtab_test(extended_symtab * symtab, int to_test, char * name);

/* tcc_get_extended_tokensym declared in libtcc.h */
/* tcc_get_extended_symbol declared in libtcc.h */

/*****************************************************************************/
/*                      Pre-compilation TokenSym Prep                        */
/*****************************************************************************/

LIBTCCAPI void tcc_prep_tokensym_list(extended_symtab * symtab);

/*****************************************************************************/
/*                      copy extended symbol into local                      */
/*****************************************************************************/

void local_stack_off();
void local_stack_on();
ST_FUNC TokenSym** symtab_tok_find(const char *str, int len); /* in tccpp.c */
TokenSym * get_local_ts_for_extended_ts(TokenSym* orig_symtab_ts, extended_symtab* orig_symtab);
int tokenstream_len (int * stream);
Sym * copy_extended_sym (extended_symtab* symtab, Sym * from, int to_tok);
void copy_extended_tokensym (extended_symtab* symtab, TokenSym * from, TokenSym * to);
void copy_ctype(CType * to_type, Sym * from, extended_symtab*symtab);
int get_local_tok_for_extended_tok(int orig_tok, extended_symtab* symtab);

/*****************************************************************************/
/*                      Extended Symbol Table Caching                        */
/*****************************************************************************/

/* tcc_set_extended_symbol is in libtcc.h; this assumes that the token exists in the trie */
