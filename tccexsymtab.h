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
	void * data;
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

/******************************************************************************/
/*                           compiled symbol lookup                           */
/******************************************************************************/

void dump_sym_names(TCCState *state);

/******************************************************************************/
/*                            extended symtab copy                            */
/******************************************************************************/

LIBTCCAPI void tcc_set_extended_symtab_callbacks (
	TCCState * s,
	extended_symtab_copy_callback new_copy_callback,
	extended_symtab_lookup_by_name_callback new_name_callback,
	extended_symtab_sym_used_callback new_sym_used_callback,
	void * data
);

int _sym_is_all_zeros(Sym * to_check);
Sym * _get_new_sym_or_def_pointer (Sym * old, Sym * new_list, int offset_of_last, Sym * stack);
Sym * get_new_symtab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last);
Sym * get_new_deftab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last);
int tokenstream_len (int * stream);
void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start);
LIBTCCAPI void tcc_delete_extended_symbol_table (
	TokenSym** my_extended_symtab
);
LIBTCCAPI char * tcc_tokensym_name (TokenSym * tokensym);
LIBTCCAPI int tcc_tokensym_tok (TokenSym * tokensym);
LIBTCCAPI long tcc_tokensym_get_id_c(TokenSym * tokensym);
LIBTCCAPI void tcc_tokensym_set_id_c(TokenSym * tokensym, long new_c);
LIBTCCAPI int tcc_tokensym_has_define (TokenSym * tokensym);
LIBTCCAPI int tcc_tokensym_has_struct (TokenSym * tokensym);
LIBTCCAPI int tcc_tokensym_has_identifier (TokenSym * tokensym);
LIBTCCAPI int tcc_tokensym_is_shareable (TokenSym * tokensym);
LIBTCCAPI int tcc_tokensym_no_extra_bits(int tok);
LIBTCCAPI TokenSym* tcc_tokensym_by_tok(int tok, TokenSym ** list);
LIBTCCAPI int tcc_tokensym_list_length (TokenSym ** list);

/*****************************************************************************/
/*                      copy extended symbol into local                      */
/*****************************************************************************/

void local_stack_off();
void local_stack_on();
ST_FUNC TokenSym** symtab_tok_find(const char *str, int len); /* in tccpp.c */
TokenSym * get_local_ts_for_extended_ts(TokenSym* orig_symtab_ts, TokenSym** orig_symtab);
int tokenstream_len (int * stream);
Sym * copy_extended_sym (TokenSym ** symtab, Sym * from, int to_tok);
void copy_extended_tokensym (TokenSym ** symtab, TokenSym * from, TokenSym * to);
void copy_ctype(CType * to_type, Sym * from, TokenSym**symtab);
int get_local_tok_for_extended_tok(int orig_tok, TokenSym** symtab);
