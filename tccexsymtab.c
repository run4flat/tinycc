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

/* Define the compressed trie used for extended table lookups. For an excellent
 * paper on the topic, see http://lampwww.epfl.ch/papers/triesearches.pdf.gz
 * This is a slightly simplified implementation of the array mapped trie. If
 * benchmarks warrant it, I may rewrite this in terms of an array compacted trie. */

/* How many slots? in 0-9, A-Z, a-z, and _, we have 10 + 26 + 26 + 1 = 63. That
 * fits nicely into an unsigned long long, leaving one extra bit that I can use
 * to indicate the presence of data in this node. The mapping of characters to
 * bits/slots is given in _c_trie_bit_offset_for_char. */

/* Returns a c_trie with a single allocated child. This should serve as the head
 * of the data structure. It does not store any data itself. The single child is
 * actually the first node in the trie. */
c_trie * c_trie_new() {
	c_trie * new_trie = tcc_malloc(sizeof(c_trie));
	/* The head of the trie is in this node's first node slot: */
	new_trie->filled_bits = 2;
	new_trie->children[0] = tcc_mallocz(sizeof(c_trie));
	return new_trie;
}

/* See http://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
 * with the explanation that begins, "I think the fastest way-without using
 * lookup tables and popcount-is the following..."  At some point, if
 * performance merits it, I can use preprocessor hacks to substitute optimzied
 * versions of this calculation. See http://danluu.com/assembly-intrinsics/. In
 * all likelihood, cache misses will be the bigger bottleneck for this,
 * switching to the array compacted trie would be the best course for speedups. */
unsigned char _c_trie_popcount (unsigned long long v) {
	/* put count of each 2 bits into those 2 bits */
	v = v - ((v >> 1) & 0x5555555555555555ULL);
	/* put count of each 4 bits into those 4 bits */
	v = (v & 0x3333333333333333ULL) + ((v >> 2) & 0x3333333333333333ULL);
	/* put count of each 8 bits into those 8 bits */
	v = (v & 0x707070707070707ULL) + ((v >> 4) & 0x707070707070707ULL);
	/* sum up those bits */
	return ((v + ((v >> 8) & 0xF000F000F000FULL)) * 0x1000100010001ULL) >> 48;
}

#define C_TRIE_HAS_DATA 1

/* Recursively free all children, then free self */
void c_trie_free(c_trie * curr) {
	unsigned char N_children = _c_trie_popcount(curr->filled_bits);
	unsigned char i = curr->filled_bits & C_TRIE_HAS_DATA;
	for (; i < N_children; i++) c_trie_free(curr->children[i]);
	tcc_free(curr);
}

/* Find the bit offset. This requires some algebra in order to fit the allowable
 * ASCII characters into just 64 bits. Since most characters in the trie are
 * going to be lowercase, I handle their calculation first. Most C programming
 * uses_underscores_rather_thanCamelCase, so I place the underscore before
 * capitals for a tiny speed boost. I then handle uppercase, and last of all
 * digits.
 * 
 * Note that the very first bit indicates that "the first element is data", and
 * is internally notated with a '$' character, though I think the character is
 * never used in the codebase.
 * 
 * Finally, note that this assumes that it is given good input. It does not
 * check for invalid characters, and could potentially give bad slots,
 * most likely for characters beyond 'Z' and before 'A', i.e. square brackets. */
unsigned char _c_trie_bit_offset_for_char (char c) {
	if (c == '$') return 0; /* data slot */
	if (c >= 'a') return c - 'a' + 1;
	if (c == '_') return 27;
	if (c >= 'A') return c - 'A' + 28;
	return c - '0' + 54;
}

/* Find the slot where this character lives and return a pointer to the child. A
 * null pointer means we couldn't find it. */
c_trie ** _c_trie_find_child (c_trie * current, char * string) {
	unsigned char bit_offset = _c_trie_bit_offset_for_char(*string);
	
	/* Figure out which bit will be occupied next. Split across two lines so
	 * that the bit shift is not accidentally truncated. */
	unsigned long long curr_bit = 1;
	curr_bit <<= bit_offset;
	
	/* See if it is in our set of buckets */
	if ((current->filled_bits & curr_bit) == 0) return NULL;
	
	/* Find the compressed offset of the child associated with this character
	 * and return it. */
	unsigned long long mask_to_popcount = 0xFFFFFFFFFFFFFFFFULL >> (64 - bit_offset);
	return &(current->children[_c_trie_popcount(current->filled_bits & mask_to_popcount)]);
}

void * c_trie_get_data (c_trie * head, char * string) {
	/* This is called with the head node, so we must go to the first child in
	 * order to begin the actual search. */
	c_trie ** child_p = head->children;
	
	/* Get the child pointer for each character in the string. */
	while(*string > 0) {
		child_p = _c_trie_find_child(*child_p, string++);
		if (child_p == NULL) return NULL;
	}
	/* At this point we have found the node that supposedly contains our data.
	 * If the data bit is set, then return the pointer in the data slot. */
	if ((*child_p)->filled_bits & C_TRIE_HAS_DATA) return (void*) (*child_p)->children[0];
	return NULL;
}

/* Allocates one more slot, if necessary, and copies data into it. */
c_trie** _c_trie_add_one_more_slot (c_trie** curr_p, c_trie * to_add, char slot_offset) {
	/* Figure out which bit will be occupied next. Split across two lines so
	 * that the bit shift is not accidentally truncated. */
	unsigned long long new_bit = 1;
	new_bit <<= slot_offset;
	
	/* c_trie objects are always allocated with room for at least one. If this
	 * reports zero room, it has room and the data can simply be added. */
	if ((*curr_p)->filled_bits == 0) {
		(*curr_p)->filled_bits = new_bit;
		(*curr_p)->children[0] = to_add;
		return &((*curr_p)->children[0]);
	}
	
	/* Otherwise, we need to allocate new space and copy over the previous
	 * children. */
	c_trie * old = *curr_p;
	unsigned char N_children = _c_trie_popcount(old->filled_bits);
	c_trie * new = tcc_mallocz(sizeof(c_trie) + N_children * sizeof(c_trie*));
	N_children++;
	
	/* Set the new filled bits */
	new->filled_bits = old->filled_bits | new_bit;
	
	/* Copy old children located in slots that come before the new one. */
	unsigned char N_before = _c_trie_popcount(old->filled_bits
		& (0xFFFFFFFFFFFFFFFFULL >> (64 - slot_offset))
	);
	if (slot_offset == 0) N_before = 0; /* corner case for data slot */
	unsigned char i;
	for(i = 0; i < N_before; i++) new->children[i] = old->children[i];
	
	/* Copy the new child and remaining children. */
	new->children[i] = to_add;
	for(i++; i < N_children; i++) new->children[i] = old->children[i-1];
	
	tcc_free(old); /* free old node */
	*curr_p = new; /* update parent's list to point to the new node */
	
	/* Return address of the newly allocated slot */
	return &(new->children[N_before]);
}

void c_trie_add_data (c_trie * head, char * string, void * data) {
	/* This is called with the head node, so we must go to the first child in
	 * order to begin the actual search. */
	c_trie ** curr_p = head->children;
	c_trie ** child_p;
	
	while(*string > 0) {
		child_p = _c_trie_find_child(*curr_p, string);
		if (child_p == NULL) {
			/* No child for this character, so create a new child. Always
			 * allocate room for one slot: it'll eventually be filled with the
			 * data pointer or with the next character. See _c_trie_add_one_more_slot
			 * for details. */
			c_trie * new_child = tcc_mallocz(sizeof(c_trie));
			
			/* Create a new slot in the current node and add the new child to it. */
			child_p = _c_trie_add_one_more_slot(curr_p, new_child,
				_c_trie_bit_offset_for_char(*string));
		}
		
		/* next character, advance through the trie */
		string++;
		curr_p = child_p;
	}
	
	/* Finally, add the new data to the "data" slot. */
	_c_trie_add_one_more_slot(curr_p, (c_trie*)data, 0);
}

/******************************************************************************/
/*                           compiled symbol lookup                           */
/******************************************************************************/

void dump_sym_names(TCCState *state) {
	Section * s;
    ElfW(Sym) *sym;
    int sym_index;
    const char *name;
    
    s = state->symtab;
	sym_index = 2;
	sym = &((ElfW(Sym) *)s->data)[sym_index];
	name = s->link->data + sym->st_name;
	while (strcmp("_etext", name) != 0) {
		printf("%s: sym_index = %d, st_shndx is %x, address is %p\n", name, sym_index, sym->st_shndx, (void*)sym->st_value);
		sym_index++;
		sym = &((ElfW(Sym) *)s->data)[sym_index];
		name = s->link->data + sym->st_name;
	}
}

/* tcc_get_next_extended_symbol_name: a simple mechanism for getting the names
 * of all of the global symbols known to the extended symbol table. */
char * tcc_get_next_extended_symbol_name(extended_symtab * symtab, int * poffset) {
	/* Increment the counter to get to the next TokenSym */
	for ((*poffset)++; symtab->tokenSym_list + *poffset < symtab->tokenSym_last; (*poffset)++) {
		TokenSym * ts = symtab->tokenSym_list[*poffset];
		if (ts->sym_identifier) return ts->str;
	}
	
	/* Reached end of list. Reset the counter and return null */
	*poffset = -1;
	return NULL;
}

void copy_extended_symbols_to_exsymtab(TCCState *state) {
	Section * s;
    ElfW(Sym) *sym;
    int sym_index;
    const char *name;
    extended_symtab* exsymtab;
    
    exsymtab = state->exsymtab;
    s = state->symtab;
	sym_index = 2;
	sym = &((ElfW(Sym) *)s->data)[sym_index];
	name = s->link->data + sym->st_name;
	while (strcmp("_etext", name) != 0) {
		/* Copy the symbol's pointer into the hash_next field of the TokenSym */
		TokenSym * ts = tcc_get_extended_tokensym(exsymtab, name);
		if (ts == NULL) {
			tcc_warning("Global symbol %s does not exist in extended symbol table; not copying\n",
				name);
		}
		else {
			ts->hash_next = (void*)sym->st_value;
		}
		/* Next iteration */
		sym_index++;
		sym = &((ElfW(Sym) *)s->data)[sym_index];
		name = s->link->data + sym->st_name;
	}
}

/* A value of NULL for exsymtab means that the extended symtab was not supposed
 * to be generated in the first place. A value of 1 means that it is supposed to
 * be created, but the state hasn't compiled yet. Otherwise, we have a fully
 * formed extended symbol table, which we can return. In that case, we assume
 * that the user takes responsibility for cleaning it up. */
LIBTCCAPI extended_symtab * tcc_get_extended_symbol_table(TCCState * s) {
	if (s->exsymtab <= (extended_symtab*)1) return NULL;
	/* clear the pointer value; otherwise we would free it, leading to a
	 * double-free situation when the user also frees it. */
	extended_symtab * to_return = s->exsymtab;
	s->exsymtab = (extended_symtab*)1;
	return to_return;
}

LIBTCCAPI TokenSym* tcc_get_extended_tokensym(extended_symtab* symtab, const char * name) {
	/* delegate to the symtab's trie */
	return (TokenSym*)c_trie_get_data(symtab->trie, (char*)name);
}

LIBTCCAPI void * tcc_get_extended_symbol(extended_symtab * symtab, const char * name) {
	TokenSym * ts = tcc_get_extended_tokensym(symtab, name);
	if (ts == NULL) return NULL;
	return (void*) ts->hash_next;
}

LIBTCCAPI int tcc_set_extended_symbol(extended_symtab * symtab, const char * name, void * pointer) {
	TokenSym * ts = tcc_get_extended_tokensym(symtab, name);
	if (ts == NULL) return 0; /* failed */
	ts->hash_next = pointer;
	return 1; /* succeeded */
}

/******************************************************************************/
/*                            extended symtab copy                            */
/******************************************************************************/

/* The user may want fine-grained control over the order of symbol table lookup.
 * Thus, I provide a set of callbacks to look for names, add symbols to compiler
 * contexts, and prep the compiler state before things get started. */
LIBTCCAPI void tcc_set_extended_symtab_callbacks (
	TCCState * s,
	extended_symtab_lookup_by_name_callback new_name_callback,
	extended_symtab_sym_used_callback new_sym_used_callback,
	extended_symtab_prep_callback new_prep_callback,
	void * data
) {
	s->symtab_name_callback = new_name_callback;
	s->symtab_sym_used_callback = new_sym_used_callback;
	s->symtab_prep_callback = new_prep_callback;
	s->symtab_callback_data = data;
}

LIBTCCAPI void tcc_save_extended_symtab(TCCState * s) {
	if (s->exsymtab == NULL) s->exsymtab = (extended_symtab*)1;
}

/* A test that indicates that the next field of this Sym is not a Sym, and must
 * be copied verbatim. */
int _type_ref_is_not_Sym(Sym * from) {
	return ((from->type.t & VT_STATIC) && (from->r & VT_SYM));
}

Sym * _get_new_sym_or_def_pointer (Sym * old, Sym * new_list, int offset_of_last, Sym * stack) {
	/* We assume that old IS NOT null; this must be checked by the higher-level
	 * functions that call this one. */
	
	/* Determine the offset of the to-be-generated list of symbols and return the
	 * pointer to that offset. Note that the list will only include non-extended
	 * symbols, so we do not increment our offset when we encounter one of those. */
	int offset = 0;
	while(stack != NULL) {
		/* Return pointer to the new symbol's location if found */
		if (stack == old) return new_list + offset;
		/* Advance to the next symbol */
		if (stack->v < SYM_EXTENDED) offset++;
		stack = stack->prev;
	}
	return NULL;
}

Sym * get_new_symtab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last) {
	/* Handle the null case up-front */
	if (old == NULL) return NULL;
	
	/* Check the global symbol stack. */
	Sym * to_return = _get_new_sym_or_def_pointer(old, new_list, offset_of_last, global_stack);
	if (NULL != to_return) return to_return;
	
	/* If we're here, we couldn't find the symbol on the global stack. This
	 * means it's an anonymous symbol, i.e. in a function declaration. Allocate
	 * a new Sym and attach it to *our* anonymous stack. */
	Sym * ll = new_list + offset_of_last - 1;
	while(ll->prev != NULL) ll = ll->prev;
	to_return = tcc_mallocz(sizeof(Sym));
	ll->prev = to_return;
	
	/* Fill in the values, as appropriate. See notes under the symbol stack
	 * copying for explanations. I suspect (highly) that this is only used in
	 * function argument lists, in which case the values of most fields don't
	 * matter. I know that the .t and .next fields are important, however, so
	 * I'll be sure to copy those. */
	/* XXX working here - check that assumption at some point with valgrind */
	to_return->r = old->r;
	to_return->type.t = old->type.t;
	int btype = old->type.t & VT_BTYPE;
	if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
		to_return->type.ref
			= get_new_symtab_pointer(s, old->type.ref, new_list, offset_of_last);
if (old->type.ref == NULL) printf("old->type.ref is null!\n");
if (to_return->type.ref == NULL) printf("to_return->type.ref is null!\n");
	}
	if (btype == VT_FUNC) to_return->c = 0;
	else to_return->c = old->c;
	to_return->next
		= get_new_symtab_pointer(s, old->next, new_list, offset_of_last);
	return to_return;
}

Sym * get_new_deftab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last) {
	/* Handle the null case up-front */
	if (old == NULL) return NULL;
	
	/* Otherwise use the non-extended symbol lookup */
	Sym * to_return = _get_new_sym_or_def_pointer(old, new_list, offset_of_last, define_stack);
	if (to_return != NULL) return to_return;
	
	/* If we're here, we ran into trouble: print out a diagnostic */
	printf("In %s line %d, unable to locate #define symbol offset for old address %p:\n", __FILE__, __LINE__, old);
	printf("  Symbol token: %X\n", old->v);
	printf("  Assembler label at address %p\n", old->asm_label);
	printf("  Associated register %lX\n", old->r);
	printf("  Associated number %lX\n", old->c);
	printf("  Type.t %X\n", old->type.t);
	printf("  Type.ref %p\n", old->type.ref);
	printf("  next symbol pointer %p\n", old->next);
	printf("  previous symbol in stack at address %p\n", old->prev);
	printf("  previous symbol for this token at address %p\n", old->prev_tok);
	return NULL;
}

/* Make a complete copy of the token-symbol table, the symbol stack, and the
 * define stack, though the user only thinks they have the token symbol table. */
void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start) {

    /* Do nothing if we have an empty TCCState. */
    if (NULL == s) return;
	
    int i;
    
    int N_tokens = tok_ident - TOK_IDENT;
    /* Room for the first TokenSym is included in the struct definition, so I
     * need to allocate room for the extended symtab plus N_tokens - 1. */
/*struct extended_symtab {
	Sym * sym_list;
	Sym * sym_last;
	Sym * def_list;
	Sym * def_last;
	c_trie * trie;
	int tok_start;
	TokenSym ** tokenSym_last;
	TokenSym [1] tokenSym_list;
     */
    
    extended_symtab * to_return = tcc_malloc(sizeof(extended_symtab)
		+ sizeof(void*) * (N_tokens - 1));
	to_return->tok_start = tok_start;
	
	/* Allocate the trie */
	to_return->trie = c_trie_new();
    
    /********* symbol stack *********/
    
    /* Calculate the number of symbols */
    Sym * curr_Sym = global_stack;
    int N_Syms = 0;
    while(curr_Sym != NULL) {
    	/* We only track non-extended symbols */
		if (curr_Sym->v < SYM_EXTENDED) N_Syms++;
		/* Step to the next symbol in the list */
		curr_Sym = curr_Sym->prev;
	}
    /* In addition to the sym list just counted, we need two more. We need a
     * sym that is all zeros (which we put as the last item), and we need a
     * sym that serves as the head of our anonymous symbol stack. The anonymous
     * symbol stack will be located at offset N_Syms - 1, so we have to
     * increment N_Syms by one.
     * working here - is this reasoning correct? */
    N_Syms++;
    Sym * sym_list = to_return->sym_list = tcc_malloc(sizeof(Sym) * (N_Syms + 1));
    to_return->sym_last = sym_list + N_Syms;
    
	/* Zero out last sym */
    sym_list[N_Syms-1].v = sym_list[N_Syms].v = 0;
    sym_list[N_Syms-1].asm_label = sym_list[N_Syms].asm_label = NULL;
    sym_list[N_Syms-1].r = sym_list[N_Syms].r = 0;
    sym_list[N_Syms-1].c = sym_list[N_Syms].c = 0;
    sym_list[N_Syms-1].type.t = sym_list[N_Syms].type.t = 0;
    sym_list[N_Syms-1].type.ref = sym_list[N_Syms].type.ref = NULL;
    sym_list[N_Syms-1].next = sym_list[N_Syms].next = NULL;
    sym_list[N_Syms-1].prev = sym_list[N_Syms].prev = NULL;
    sym_list[N_Syms-1].prev_tok = sym_list[N_Syms].prev_tok = NULL;
    
    /* Copy the Sym list */
    for (curr_Sym = global_stack, i = 0; i < N_Syms-1; curr_Sym = curr_Sym->prev) {
		/* See tcc.h around line 425 for descriptions of some of the fields.
		 * See also tccgen.c line 5987 to see what needs to happen for function
		 * declarations to work properly (and, in turn, line 446 for how to
		 * push a forward reference). */
		
		/* Copy the v value (token id). This will not be copied later, so keep
		 * things simple for now and simply strip out the extended flag. */
		sym_list[i].v = curr_Sym->v & ~SYM_EXTENDED;
		 
		/* Copy the assembler label */
		if (curr_Sym->asm_label != NULL) {
			int asm_label_len = strlen(curr_Sym->asm_label) + 1;
			sym_list[i].asm_label = tcc_malloc(asm_label_len);
			memcpy(sym_list[i].asm_label, curr_Sym->asm_label,
				asm_label_len);
		}
		else {
			sym_list[i].asm_label = NULL;
		}
		
		/* associated register. For variables, I believe that the low bits
		 * specify the register size that can hold the value while high bits
		 * indicate storage details (VT_SYM, VT_LVAL, etc). For function types,
		 * however, this gets cast as an AttributeDef and queried for function
		 * attributes; so far, I have only seen the .r field queried for the
		 * FUNC_CALL field. It matters little; copying the whole long is easy
		 * and it seems that everything works fine when it is the same for
		 * consuming contexts as for the original compilation context. */
		sym_list[i].r = curr_Sym->r;
		
		/* Set the type. Judging by the constants in tcc.h and code that
		 * uses this field, I'm pretty sure that the low bits in the .t field
		 * tells tcc how to load the data into a register. The high bits seem to
		 * indicate storage details, such as VT_EXTERN. Since that is not
		 * something that can be extended at runtime, I should be able to copy
		 * the value as-is and add an extern flag for variables and functions. */
		sym_list[i].type.t = curr_Sym->type.t;
		/* After compilation, functions and global variables point to hard
		 * locations in memory. Consuming contexts should think of these as
		 * having external storage, which is reflected in the VT_EXTERN bit of
		 * the type.t field. */
		int btype = curr_Sym->type.t & VT_BTYPE;
		if (btype == VT_FUNC
			|| sym_list[i].r & (VT_SYM | VT_LVAL | VT_CONST) /* global variables ?? */
		) sym_list[i].type.t |= VT_EXTERN;
		
		/* The type.ref field contains something useful only if the basic type
		 * is a pointer, struct, or function. See code from tccgen's
		 * compare_types for details. */
		if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
			if (_type_ref_is_not_Sym(sym_list + i)) {
				/* If we have a static symbol, copy its pointer directly, since
				 * after relocation it is no longer a Sym pointer at all! */
				sym_list[i].type.ref = curr_Sym->type.ref;
			}
			else {
				/* Otherwise it's safe: get a new Sym for it. */
				sym_list[i].type.ref
					= get_new_symtab_pointer(s, curr_Sym->type.ref, sym_list, N_Syms);
			}
		}
		
		/* Copy the c field, the "associated number." For functions, this is
		 * one of FUNC_NEW, FUNC_OLD, or FUNC_ELLIPSIS. For structs, this is
		 * the size (in bytes), and for struct members it is the byte offset
		 * of the member, according to the end of struct_decl().
		 * Line 5982 of tccgen.c seems to suggest that this needs to be
		 * **negative** and we need VT_CONST in order to get external linkage.
		 * However, it seems to work if I simply set it to zero for functions
		 * and global variables, so I'm going with that. This almost certainly
		 * needs to be more nuanced. */
		if (btype == VT_FUNC
			|| sym_list[i].r & (VT_SYM | VT_LVAL | VT_CONST)
		) sym_list[i].c = 0;
		else sym_list[i].c = curr_Sym->c;
		
		/* Copy the next symbol field. Labels and gotos are tracked in a
		 * separate stack, so for these Symbols we focus on next, not
		 * jnext. The next field (I seem to recall) is used in storing
		 * argument lists, so it needs to be copied for function
		 * types. I believe it can be copied anonymously. */
		sym_list[i].next
			= get_new_symtab_pointer(s, curr_Sym->next, sym_list, N_Syms);
		
		/* These are only needed for symbol table pushing/popping, so I
		 * should be able to safely set them to null. */
		sym_list[i].prev = NULL;
		sym_list[i].prev_tok = NULL;
		
		/* Move on to the next symbol. */
		i++;
	}
    
    /********* define stack *********/
    
    /* Calculate the number of defines */
    Sym * curr_Def = define_stack;
    int N_Defs = 0;
    while(curr_Def != define_start) {
		curr_Def = curr_Def->prev;
		N_Defs++;
	}
    /* Allocate the Def list */
    Sym * def_list = to_return->def_list = tcc_malloc(sizeof(Sym) * (N_Defs + 1));
    to_return->def_last = def_list + N_Defs;
    
	/* Zero out last sym */
    def_list[N_Defs].v = 0;
    def_list[N_Defs].asm_label = NULL;
    def_list[N_Defs].r = 0;
    def_list[N_Defs].d = NULL;
    def_list[N_Defs].type.t = 0;
    def_list[N_Defs].type.ref = NULL;
    def_list[N_Defs].next = NULL;
    def_list[N_Defs].prev = NULL;
    def_list[N_Defs].prev_tok = NULL;
    
    /* Copy the define list */
    for (curr_Def = define_stack, i = 0; i < N_Defs; i++, curr_Def = curr_Def->prev) {
		/* See above for descriptions of some of the fields. */
		 
		/* Convert the symbol's token index. */
		def_list[i].v = curr_Def->v & ~SYM_EXTENDED;
		
		/* assembler label should be null for preprocessor stuff */
		if (curr_Def->asm_label != NULL) {
			tcc_warning("Unexpected assembler label for macro symbol");
		}
		def_list[i].asm_label = NULL;
		
		/* As far as I can tell, the 'r' field is not used by
		 * preprocessor macros. Just copy it. */
		def_list[i].r = curr_Def->r;
		
		/* Copy the tokenstream if it exists */
		if (curr_Def->d != NULL) {
			int * str = curr_Def->d;
			int len = tokenstream_len(str);
			def_list[i].d = tcc_malloc(sizeof(int) * len);
			/* The token ids used in the original compiling context are in the
			 * same order as the final extended symbol table, so I can just copy
			 * the token stream verbatim! */
			memcpy(def_list[i].d, curr_Def->d, sizeof(int) * len);
		}
		else {
			def_list[i].d = NULL;
		}
		
		/* Set the type. define_push and parse_define indicate that this
		 * will be either MACRO_OBJ or MACRO_FUNC. */
		def_list[i].type.t = curr_Def->type.t;
		/* Macro types should be null; issue a warning if this is not
		 * the case, just so we're aware. */
		if (curr_Def->type.ref != NULL) {
			tcc_warning("Unexpected type ref for macro symbol");
		}
		def_list[i].type.ref = NULL;
		
		/* Copy the macro arguments. All macro arguments are placed
		 * on the define stack, according to the sym_push2 in
		 * parse_define from tccpp.c. We only need to update this Sym's
		 * next; *it's* next will be updated when it comes up in this
		 * loop. */
		def_list[i].next
			= get_new_deftab_pointer(s, curr_Def->next, def_list, N_Defs);
		
		/* These are only needed for symbol table pushing/popping and
		 * label identification. Since these Sym objects will do no
		 * such things, I should be able to safely set them to null. */
		def_list[i].prev = NULL;
		def_list[i].prev_tok = NULL;
	}
    
    /* Set the tail pointer, which points to the first address past the
     * last element. */
    to_return->tokenSym_last = to_return->tokenSym_list + N_tokens;
    
    /********* TokenSym list *********/
    
    /* Copy the tokens */
	for (i = 0; i < N_tokens; i++) {
		TokenSym * tok_copy = table_ident[i];
		int tokensym_size = sizeof(TokenSym) + tok_copy->len;
		TokenSym * tok_sym = to_return->tokenSym_list[i]
			= tcc_malloc(tokensym_size);
		
		/* Follow the code from tok_alloc_new in tccpp.c */
		tok_sym->tok = tok_copy->tok;
		tok_sym->sym_define
			= get_new_deftab_pointer(s, tok_copy->sym_define, def_list, N_Defs);
		tok_sym->sym_label = NULL; /* Not copying labels */
		tok_sym->sym_struct
			= get_new_symtab_pointer(s, tok_copy->sym_struct, sym_list, N_Syms);
		tok_sym->sym_identifier
			= get_new_symtab_pointer(s, tok_copy->sym_identifier, sym_list, N_Syms);
		tok_sym->len = tok_copy->len;
		tok_sym->hash_next = NULL;
		memcpy(tok_sym->str, tok_copy->str, tok_copy->len);
		tok_sym->str[tok_copy->len] = '\0';
		
		/* Add this to the trie */
		c_trie_add_data(to_return->trie, tok_sym->str, tok_sym);
	}

	/* Store the extended symtab */
	s->exsymtab = to_return;
}

/* Frees memory associated with a copied extended symbol table. For a
 * description of the structure of the allocated memory, see the copy
 * function above. */
LIBTCCAPI void tcc_delete_extended_symbol_table (extended_symtab * symtab) {
	Sym * sym_to_delete = symtab->sym_list;
	Sym * last = symtab->sym_last;
	
	/* clear out the anonymous symbol stack */
	Sym * curr_sym = (last - 1)->prev;
	Sym * prev_sym;
	while(curr_sym != NULL) {
		prev_sym = curr_sym->prev;
		tcc_free(curr_sym);
		curr_sym = prev_sym;
	}
	
	/* clear out the symbol list */
	while(sym_to_delete < last) {
		tcc_free(sym_to_delete->asm_label);
		sym_to_delete++;
	}
	tcc_free(symtab->sym_list);
	
	/* clear out the define list */
	sym_to_delete = symtab->def_list;
	last = symtab->def_last;
	while(sym_to_delete < last) {
		/* free the token stream */
		tcc_free(sym_to_delete->d);
		sym_to_delete++;
	}
	tcc_free(symtab->def_list);
	
	/* Clear out the trie */
	c_trie_free(symtab->trie);
	
	/* Clear out the allocated TokenSym pointers */
	TokenSym** ts_to_delete = symtab->tokenSym_list;
	TokenSym** done = symtab->tokenSym_last;
	while (ts_to_delete < done) {
		tcc_free(*ts_to_delete);
		ts_to_delete++;
	}
	
	/* Clear out the full memory allocation. */
	tcc_free(symtab);
}

/* A function that performs a number of tests for me. I only export a single
 * function to avoid cluttering up the TCC API. */
enum {
	TS_TEST_GET_TOK,
	TS_TEST_HAS_DEFINE,
	TS_TEST_HAS_STRUCT,
	TS_TEST_HAS_IDENTIFIER
};
LIBTCCAPI int tcc_extended_symtab_test(extended_symtab_p symtab, int to_test, char * name) {
	/* Get the tokenSym by the given name */
	TokenSym * ts = c_trie_get_data(symtab->trie, name);
	if (ts == NULL) return 0;
	
	/* Perform the requested test */
	switch(to_test) {
		case TS_TEST_GET_TOK:
			return ts->tok;
		case TS_TEST_HAS_DEFINE:
			return ts->sym_define != NULL;
		case TS_TEST_HAS_IDENTIFIER:
			return ts->sym_identifier != NULL;
		case TS_TEST_HAS_STRUCT:
			return ts->sym_struct != NULL;
	}
	return 0;
}

/*****************************************************************************/
/*                      Pre-compilation TokenSym Prep                        */
/*****************************************************************************/

LIBTCCAPI void tcc_prep_tokensym_list(extended_symtab * symtab) {
	int i, end;
	end = symtab->tok_start - TOK_IDENT;
	for (i = 0; i < end; i++) {
		TokenSym_p local_ts = table_ident[i];
		TokenSym_p ext_ts = symtab->tokenSym_list[i];
		/* Skip if we've already copied something for this TokenSym from another
		 * extended symbol table. */
		if (local_ts->sym_struct || local_ts->sym_identifier || local_ts->sym_define) continue;
		
		/* Copy if there's something interesting to copy */
		if (ext_ts->sym_struct || ext_ts->sym_identifier
			|| (ext_ts->sym_define && ext_ts->sym_define->d)
		) {
			copy_extended_tokensym(symtab, ext_ts, local_ts);
		}
	}
}

/*****************************************************************************/
/*                      copy extended symbol into local                      */
/*****************************************************************************/

/* Provide a mechanism for turning the local_stack off and back on outside of
 * the current static file scope. */
ST_DATA Sym * local_stack_backup;
void local_stack_off() {
	local_stack_backup = local_stack;
	local_stack = NULL;
}
void local_stack_on() {
	local_stack = local_stack_backup;
}

/* The define token stream is a series of bytes. They are collections of
 * integer => data pairs, where the integer indicates the type (and thus size)
 * of the data that follows. For example, if we encounter types TOK_PPNUM,
 * TOK_STR, or TOK_LSTR, then the next few bytes are a CString struct (mostly
 * filled with NULLs) followed by the associated character string. It gets
 * complicated, but has only a handful of special cases to handle. The formats
 * of the stream are codified in tok_str_add2, which is defined in tccpp.c. */
int tokenstream_len (int * stream) {
	int len = 0;
	while(stream[len] != 0) {
		/* One for the type */
		len++;
		switch(stream[len-1]) {
			case TOK_CINT: case TOK_CUINT: case TOK_CCHAR:
			case TOK_LCHAR: case TOK_CFLOAT: case TOK_LINENUM:
				len++;
				break;
			case TOK_PPNUM: case TOK_STR: case TOK_LSTR:
				{
					CString *cstr = (CString *)(stream + len);
					/* Note this right shift assumes 32 bit integers */
					len += (sizeof(CString) + cstr->size + 3) >> 2;
				}
				break;
			case TOK_CDOUBLE: case TOK_CLLONG: case TOK_CULLONG:
		#if LDOUBLE_SIZE == 8
			case TOK_CLDOUBLE:
		#endif
				len += 2;
				break;
		#if LDOUBLE_SIZE == 12
			case TOK_CLDOUBLE:
				len += 3;
		#elif LDOUBLE_SIZE == 16
			case TOK_CLDOUBLE:
				len += 4;
		#elif LDOUBLE_SIZE != 8
		#error add long double size support
		#endif
				break;
			
			default:
				/* Default is a single token (integer), which doesn't require
				 * any additional bytes. So do nothing. */
				break;
		}
	}
	/* add one for the zero byte */
	return len + 1;
}

void copy_extended_tokensym (extended_symtab * symtab, TokenSym * from, TokenSym * to) {
	/* Mark this token as extended. This will cause a symbol-used callback to be
	 * fired the first time this token is used in reference to a symbol (at
	 * which point the extended flag will be cleared). */
	to->tok |= SYM_EXTENDED;
	/* We need to copy over the following fields:
	 *  - sym_define
	 *  - sym_struct
	 *  - sym_identifier
	 * These fields are OK as they are:
	 *  - str has already been allocated and copied
	 *  - len has already been set to the string's length
	 *  - sym_label should already be null
	 *  - hash_next is probably null, and ought not be modified
	 */
	 
	/***** sym_struct and sym_identifier copy *****/
	to->sym_struct = copy_extended_sym(symtab, from->sym_struct, to->tok);
	to->sym_identifier = copy_extended_sym(symtab, from->sym_identifier, to->tok);
	
	/***** sym_define copy *****/
	/* There may be no sym_define, or it may have been undef'd. Note that
	 * something which is just defined (and subsequently used in #ifdef
	 * statements) have a non-null d field, which points to an int string that
	 * only contains a single zero-valued int. */
	if (from->sym_define == NULL || from->sym_define->d == NULL) {
		to->sym_define = NULL;
	}
	/* Otherwise, we need to copy it and update all of the token values. I
	 * refrain from using the tok_str_* functions because I already have the
	 * entire token stream and can easily get its length. After copying the
	 * token stream bit-for-bit, I go back and re-assign the token values
	 * referring to barewords so that they refer to tokens in the current
	 * compilation context rather than the original extended symbol table. */
	else {
		/* Get the length and copy the original */
		int * from_stream = from->sym_define->d;
		int len = tokenstream_len(from_stream);
		int * to_stream = tcc_malloc(sizeof(int) * len);
		memcpy(to_stream, from_stream, sizeof(int) * len);
		
		/* Get the starting token's id, used for correct extended table lookups */
		int tok_start = symtab->tokenSym_list[0]->tok
			& ~(SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM);
		
		/* Update TokenSym references to point to TokenSyms in the current
		 * compiler context. Most of this code involves stepping over the other
		 * possible elements of the token stream. See tokenstream_len for
		 * details. */
		for (len = 0; from_stream[len] != 0;len++) {
			switch(from_stream[len]) {
				case TOK_CINT: case TOK_CUINT: case TOK_CCHAR:
				case TOK_LCHAR: case TOK_CFLOAT: case TOK_LINENUM:
					/* Skip the next stream value */
					len++;
					break;
				case TOK_PPNUM: case TOK_STR: case TOK_LSTR:
				{
					CString *cstr = (CString *)(from_stream + len + 1);
					/* Note this right shift assumes 32 bit integers */
					len += (sizeof(CString) + cstr->size + 3) >> 2;
					/* Naively, I should set up cstr to be usable for later
					 * preprocessor expansions. See tok_str_add2 in tccpp.c
					 * for details. However, the memcpy performed above
					 * already did that! So I'm done. */
					break;
				}
				case TOK_CDOUBLE: case TOK_CLLONG: case TOK_CULLONG:
			#if LDOUBLE_SIZE == 8
				case TOK_CLDOUBLE:
			#endif
					len += 2;
					break;
			#if LDOUBLE_SIZE == 12
				case TOK_CLDOUBLE:
					len += 3;
			#elif LDOUBLE_SIZE == 16
				case TOK_CLDOUBLE:
					len += 4;
			#elif LDOUBLE_SIZE != 8
			#error add long double size support
			#endif
					break;
				default:
					if (from_stream[len] >= tok_start) {
						/* This is the case for an arbitrary token. Get a local
						 * token and replace the token stream's value with the
						 * local token's tok id. */
						int from_tok = from_stream[len] & ~SYM_EXTENDED;
						to_stream[len] = get_local_ts_for_extended_ts(
							symtab->tokenSym_list[from_tok - tok_start], symtab)->tok;
					}
					/* Any token value less than tok_start refers to a value in
					 * the symbol table that is pre-defined, such as the C
					 * language key words (struct, case) and the ASCII letters. */
					
					
					break;
			}
		}
		/* We have copied the token stream, but we still need to copy the
		 * argument list (for macro functions). */
		Sym *first_arg, *curr_from_arg, *newest_arg, **p_curr_arg;
		first_arg = NULL;
		p_curr_arg = &first_arg;
		for (curr_from_arg = from->sym_define->next; curr_from_arg != NULL;
			curr_from_arg = curr_from_arg->next
		) {
			/* Get local TokenSym associated with curr_from_arg */
			int from_tok = (curr_from_arg->v & ~SYM_FIELD) - tok_start;
			TokenSym * local_ts = get_local_ts_for_extended_ts(symtab->tokenSym_list[from_tok],
				symtab);
			/* Add the argument to the local define stack and move the chains */
			newest_arg = sym_push2(&define_stack, local_ts->tok | SYM_FIELD,
				curr_from_arg->type.t, 0);
			*p_curr_arg = newest_arg;
			p_curr_arg = &newest_arg->next;
		}
		
		/* Now that we have all the moving parts, add the preprocessor to the
		 * current compilation context. */
		define_push(to->tok, from->sym_define->type.t, to_stream, first_arg); /* sym_define is now set */
	}
}

/* Copy the CType information from an extended sym into a local CType. The hard
 * part here is finding the local tokensym associated with the type.ref field,
 * which is only an issue if the type is a pointer, struct, or function. */
#define copy_ctype(to_type, from, symtab) do { \
	int btype = from->type.t & VT_BTYPE; \
	to_type.t = from->type.t; \
	if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) { \
		/* Copy pointers that are non-Sym pointers directly */ \
		if (_type_ref_is_not_Sym(from)) { \
			to_type.ref = from->type.ref; \
		} \
		/* Get the from->type.ref's token and look for it here */ \
		else if (from->type.ref->v & SYM_FIRST_ANOM) { \
			/* Anonymous symbol; just copy it. */ \
			to_type.ref = copy_extended_sym(symtab, from->type.ref, \
				anon_sym++ | (from->type.ref->v & (SYM_STRUCT | SYM_FIELD))); \
		} \
		else if (from->type.ref->v == SYM_FIELD) { \
			/* Anonymous symbol; just copy it. */ \
			to_type.ref = copy_extended_sym(symtab, from->type.ref, SYM_FIELD); \
		} \
		else { \
			/* Not anonymous: get the tokensym */ \
			int tok_start = symtab->tokenSym_list[0]->tok & ~(SYM_STRUCT | SYM_FIELD); \
			int tok_from = from->type.ref->v & ~(SYM_STRUCT | SYM_FIELD); \
			TokenSym* orig_ts = symtab->tokenSym_list[tok_from - tok_start]; \
			TokenSym* local_ts = get_local_ts_for_extended_ts(orig_ts, symtab); \
			if (btype == VT_STRUCT) to_type.ref = local_ts->sym_struct; \
			else to_type.ref = local_ts->sym_identifier; \
		} \
	} \
} while(0)

/* Gets a local TokenSym pointer for a given extended TokenSym of the given tok,
 * and adds the extended tok's flags to the local tok's id. */
int get_local_tok_for_extended_tok(int orig_tok, extended_symtab* symtab) {
	int tok_start = symtab->tokenSym_list[0]->tok & ~(SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM);
	int orig_tok_no_fields = orig_tok & ~(SYM_STRUCT | SYM_FIELD);      /* strip flags  */
	
	/* special case for ordinary tokens that exist in all compiler contexts,
	 * including "data", "string", and others. */
	if (orig_tok_no_fields < tok_start) return orig_tok;
	
	TokenSym* orig_ts = symtab->tokenSym_list[orig_tok_no_fields - tok_start];         /* get ext ts   */
	TokenSym* local_ts = get_local_ts_for_extended_ts(orig_ts, symtab); /* get local ts */
	return local_ts->tok | (orig_tok & (SYM_STRUCT | SYM_FIELD));       /* add flags    */
}

Sym * copy_extended_sym (extended_symtab * symtab, Sym * from, int to_tok) {
	if (from == NULL) return NULL;
	
	/* Copy the flags and CType from the "from" sym and push on the symbol stack */
	to_tok |= from->v & (SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM);
	CType to_type;
	copy_ctype(to_type, from, symtab);
	Sym * s = sym_push(to_tok, &to_type, from->r, from->c);
	
	/* Copy the assembler label, if present */
	if (from->asm_label != NULL) {
		int asm_label_len = strlen(from->asm_label) + 1;
		s->asm_label = tcc_malloc(asm_label_len);
		memcpy(s->asm_label, from->asm_label, asm_label_len);
	}
	
	/* All done unless we have a next field to copy as well. */
	if (from->next == NULL) return s;
	
	/* Copy the linked list started in the next field. Much of this code
	 * resembles copy_ctype, unfortunately. */
	Sym * from_next = from->next;
	Sym **psnext = &s->next;
	while (from_next) {
		if (from_next->v & SYM_FIRST_ANOM) {
			/* Anonymous symbol; not attached to a TokenSym, so just copy it. */
			*psnext = copy_extended_sym(symtab, from_next,
				anon_sym++ | (from_next->v & (SYM_STRUCT | SYM_FIELD)));
			/* copy_extended_sym is a recursive function call which copied the
			 * remaining elements of the next chain. Thus, we're done. */
			return s;
		}
		else if (from_next->v == SYM_FIELD) {
			/* This is an anonymous symbol associated with pointers, arrays, and
			 * function declarations (tccgen.c in post_type). As above, just
			 * copy it. */
			*psnext = copy_extended_sym(symtab, from_next, SYM_FIELD);
			/* copy_extended_sym is a recursive function call which copied the
			 * remaining elements of the next chain. Thus, we're done. */
			return s;
		}
		
		/* Figure out the new token. This can be from's token if the token
		 * exists in all compiler contexts. Such tokens include "data",
		 * "string", and others; assume this until proven otherwise. */
		int new_tok = from_next->v;
		
		/* If this is not a token in all compiler contexts... */
		int from_tok_no_fields = from_next->v & ~(SYM_STRUCT | SYM_FIELD);      /* strip flags  */
		int tok_start = symtab->tokenSym_list[0]->tok & ~(SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM);
		if (from_tok_no_fields >= tok_start) {
			TokenSym* from_ts = symtab->tokenSym_list[from_tok_no_fields - tok_start];         /* get ext ts   */
			TokenSym* local_ts = get_local_ts_for_extended_ts(from_ts, symtab); /* get local ts */
			new_tok = local_ts->tok | (from_next->v & (SYM_STRUCT | SYM_FIELD));       /* add flags    */
		}
		
		/* Push a copy of the Sym to the local symbol stack. */
		CType new_next_type;
		copy_ctype(new_next_type, from_next, symtab);
		*psnext = sym_push(new_tok, &new_next_type, from_next->r, from_next->c);
		
		/* Cycle the pointers. */
		from_next = from_next->next;
		psnext = &((*psnext)->next);
	}
	
	return s;
}
