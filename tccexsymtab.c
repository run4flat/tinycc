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
	
	/* Handle the case where the data slot is already used */
	if ((*curr_p)->filled_bits & 1) {
		tcc_warning("Attempting to add data to ctrie node that already has data!\n");
		return;
	}
	
	/* Finally, add the new data to the "data" slot. */
	_c_trie_add_one_more_slot(curr_p, (c_trie*)data, 0);
}

/****************************************************************************/
/*                                 ram tree                                 */
/****************************************************************************/

/* This provides a mechanism for mapping a set of old pointers to a set of
 * new pointers. Assuming that a collection of data structures are being
 * copied, this basically provides an interface to say, "What is the new
 * address for this old address?"
 * 
 * The current implementation is pretty basic and can probably be optimized.
 * Lookup and insertion time is constant, but it uses O(N log(N)) memory for
 * a lookup table consisting of N pointers. It builds a two-way branching
 * data structure based on each bit in the pointer address being looked up.
 * To improve, I can probably use a hash table. For a discussion of good
 * hashing functions for pointers, see this:
 * http://stackoverflow.com/questions/20953390/what-is-the-fastest-hash-function-for-pointers
 * 
 * As currently implemented, you should create a new ramtree with
 * ramtree_new and free the memory associated with your ramtree with
 * ramtree_free. The data is stored in ram_tree pointers, so you would say
 * 
 *  ram_tree * my_ramtree = ram_tree_new();
 * 
 */

ram_tree * ram_tree_new() {
	return tcc_mallocz(sizeof(ram_tree));
}

/* ram_tree_get_ref: given an old pointer, returns a *reference* to the
 * new pointer, building out intermediate tree branches as necessary.
 * The reason this returns a reference to the pointer rather than the
 * pointer itself is so that you can work with the result as an lvalue.
 * 
 *  void ** p_data = ram_tree_get_ref(my_ram_tree, old_ptr);
 *  if (*p_data == NULL) {
 *      *p_data = create_new_data();
 *  }
 * 
 * Algorithm: We begin at the head of our data structure and go left or
 * right depending on the bit. Thus the bits map to the *branches*, not
 * the *nodes*. From the TOP to the leaves, we will traverse N_bits
 * nodes and follow N_bits - 1 branches. The final node will also
 * contain a left and a right, but the pointers contained therein will
 * be the mapped pointers, not another node. So, my while loop should
 * take N_bits - 1 iterations.

                                      TOP
                   0                                        1
        00                  01                   10                  11
   000       001       010       011        100       101       110       111
0000 0001 0010 0011 0100 0101 0110 0111  1000 1001 1010 1011 1100 1101 1110 1111
*/

void ** ram_tree_get_ref(ram_tree * rt, void * old) {
	/* Start with most significant bit */
	unsigned long long mask = 1ULL << (sizeof(void*) * 8 - 1);
	while(mask != 1) {
		int offset = (mask & (unsigned long long)old) ? 1 : 0;
		/* Branch does not exist? allocate */
		if (rt->branches[offset] == NULL) {
			rt->branches[offset] = tcc_mallocz(sizeof(ram_tree));
		}
		/* Take step into branch */
		rt = rt->branches[offset];
		mask >>= 1;
	}
	
	/* The last layer contains the actual pointers. Return a reference
	 * to them so they can be dereferenced, and possibly modified. */
	int offset = 1 & (unsigned long long)old;
	return rt->leaves + offset;
}

/* void ** ram_tree_iterate(void * ram_tree, void ** iter_data)
 * Iterates through the ram_tree data, returning a reference to new leaf
 * with each call. The void ** iter_data is a reference to a void pointer
 * that is used by the iterator to store state between calls. You should
 * call this function like so:
 * 
 *  void * iter_data = NULL;
 *  do {
 *      void ** data_ref = ram_tree_iterate(ram_tree, &iter_data);
 *      ...
 *  } while (iter_data != NULL);
 * 
 * For example, to count the number of entries, you could do this
 * 
 *  void * iter_data = NULL;
 *  int count = 0;
 *  do {
 *      count++;
 *      ram_tree_iterate(ram_tree, &iter_data);
 *  } while (iter_data != NULL;
 * 
 * To free data referenced by all leaf pointers, use this
 * 
 *  void * iter_data = NULL;
 *  void ** ptr_ref;
 *  do {
 *      ptr_ref = ram_tree_iterate(ram_tree, &iter_data);
 *      free(*ptr_ref);
 *  } while (iter_data != NULL;
 *
 * State is allocated on the heap. For the moment, the only way to free
 * the state information is to iterate through all of the data.
 */

typedef struct {
	ram_tree * node;
	int level;
} rt_bypassed_data;

void ** ram_tree_iterate(ram_tree * rt, void ** p_bypassed) {
	/* dereference the pointer they passed in */
	rt_bypassed_data * bypassed = *p_bypassed;
	
	/* If bypassed is not initialized, then we allocate memory and
	 * initialize things so that we start at the top node. */
	if (bypassed == NULL) {
		/* Allocate plenty of room, the first element of which is a
		 * "null" delimiter so we know when there are no more bypassed
		 * nodes to worry about. (If we have a 32-bit system, we want
		 * room for 33 rt_bypassed_data structs at our disposal.) */
		bypassed = tcc_malloc(sizeof(rt_bypassed_data) * (sizeof(void*) + 1));
		bypassed->node = NULL;
		bypassed->level = -1; /* doesn't matter */
		
		/* Moving forward, bypassed will always point to the *end* of
		 * the list of bypassed nodes, making it a lifo. Since the whole
		 * ram tree is unexplored, add the top node to the list by
		 * moving to the second slot and adding the appropriate
		 * information. */
		bypassed++;
		bypassed->node = rt;
		bypassed->level = sizeof(void*) - 1;
	}
	
	/* The leaves are void*. We will return a reference to one of those */
	void ** to_return;
	
	/* Pop the most recently bypassed node and level off the lifo */
	int level = bypassed->level;
	ram_tree * curr_node = bypassed->node;
	bypassed--;
	
	if (level == 0) {
		/* If the most recent node is at depth zero, then it means that
		 * we need to return a reference to the node's right leaf. */
		to_return = curr_node->leaves + 1;
	}
	else {
		/* If we are not dealing with that edge case, then begin heading
		 * down the ram tree from the given node. */
		for (; level > 0; level--) {
			/* if a left branch exists... */
			if (curr_node->branches[0] != NULL) {
				/* ... and we're bypassing a right branch ... */
				if (curr_node->branches[1] != NULL) {
					/* ... push the bypassed node onto our lifo */
					bypassed++;
					bypassed->node = curr_node->branches[1];
					bypassed->level = level - 1;
				}
				/* ... and under all circumstances, move to the left node */
				curr_node = curr_node->branches[0];
			}
			/* If a left branch does not exist, move to the right node */
			else curr_node = curr_node->branches[1];
		}
		
		/* Out here we are sitting on the final node. Again perform the
		 * left/right leaf dance seen just above, but this time we do it
		 * to figure out the return leaf. If a left leaf exists... */
		if (curr_node->leaves[0] != NULL) {
			/* ... and we also have a right leaf ... */
			if (curr_node->leaves[1] != NULL) {
				/* ... then add this node to the bypassed list, with
				 * depth 0. This will trigger special handling. */
				bypassed++;
				bypassed->node = curr_node;
				bypassed->level = 0;
			}
			/* ... and under all circumstances, return a reference to
			 * the left leaf */
			to_return = curr_node->leaves;
		}
		/* If a left leaf does not exist, return a reference to the
		 * right leaf. */
		else to_return = curr_node->leaves + 1;
	}
	
	/* The last item in our tree will have no more bypassed nodes, in
	 * which case the bypassed pointer will be sitting on our null
	 * element at the head of the lifo. In that case, free the lifo. */
	if (bypassed->node == NULL) {
		tcc_free(bypassed);
		bypassed = NULL;
	}
	
	/* make sure p_bypassed refers to the end of the lifo (or NULL) */
	*p_bypassed = bypassed;
	
	return to_return;
}

/* ram_tree_free(void * ram_tree)
 * Frees memory associated with a ram_tree. Does not do anything with
 * the leaves. Use ram_tree_iterate to go through the leaves and take
 * care of memory allocations stored there.
 */

void _ram_tree_free_level(ram_tree * rt, int level) {
	/* skip if this is null or a leaf */
	if (rt == NULL || level == 0) return;
	/* free children */
	_ram_tree_free_level(rt->branches[0], level - 1);
	_ram_tree_free_level(rt->branches[1], level - 1);
	/* free self */
	tcc_free(rt);
}

void ram_tree_free(ram_tree * rt) {
	_ram_tree_free_level(rt, sizeof(void*));
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

Sym * get_new_symtab_pointer (Sym * old, ram_tree * rt) {
	/* Handle the null case up-front */
	if (old == NULL) return NULL;
	
	/* Check the global symbol stack. */
	void ** Sym_ref = ram_tree_get_ref(rt, old);
	Sym * to_return = *Sym_ref;
	if (NULL != to_return) return to_return;
	
	/* If we're here, we couldn't find the symbol on the global stack. This
	 * means it's an anonymous symbol, i.e. in a function declaration. Allocate
	 * a new Sym. */
	to_return = *Sym_ref = tcc_malloc(sizeof(Sym));
	
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
		to_return->type.ref = get_new_symtab_pointer(old->type.ref, rt);
if (old->type.ref == NULL) printf("old->type.ref is null!\n");
if (to_return->type.ref == NULL) printf("to_return->type.ref is null!\n");
	}
	if (btype == VT_FUNC) to_return->c = 0;
	else to_return->c = old->c;
	to_return->next = get_new_symtab_pointer(old->next, rt);
	return to_return;
}

Sym * get_new_deftab_pointer (Sym * old, ram_tree * rt) {
	/* Handle the null case up-front */
	if (old == NULL) return NULL;
	
	/* Otherwise use the non-extended symbol lookup */
	void ** Sym_ref = ram_tree_get_ref(rt, old);
	Sym * to_return = *Sym_ref;
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

/* Make a complete copy of the TokenSym and Sym tables, using a ram_tree
 * for the latter. */
void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start) {

    /* Do nothing if we have an empty TCCState. */
    if (NULL == s) return;
	
    int i;
    
    int N_tokens = tok_ident - TOK_IDENT;
    /* Room for the first TokenSym is included in the struct definition, so I
     * need to allocate room for the extended symtab plus N_tokens - 1. */
/*struct extended_symtab {
	ram_tree * sym_rt;
	c_trie * trie;
	int tok_start;
	TokenSym ** tokenSym_last;
	TokenSym [1] tokenSym_list;
     */
    
    extended_symtab * to_return = tcc_malloc(sizeof(extended_symtab)
		+ sizeof(void*) * (N_tokens - 1));
	to_return->tok_start = tok_start;
	
	/* Allocate the trie and ram_tree */
	to_return->trie = c_trie_new();
	ram_tree * rt = to_return->rt = ram_tree_new();
    
    void ** Sym_ref;
    Sym * new_sym;
    Sym * curr_Sym;
    
    /********* symbol stack *********/
    
    /* All Syms are stored in the leaves of the ram_tree and are
     * accessible by their old address, or via ram tree iteration. Begin
     * by allocating memory for all Syms that are currently on the
     * global stack. This way, if I *can't* later on locate a Sym with
     * get_new_symtab_pointer, I can be sure it is an anonymous Sym. */
    for (curr_Sym = global_stack; curr_Sym != NULL; curr_Sym = curr_Sym->prev) {
		Sym_ref = ram_tree_get_ref(rt, curr_Sym);
		*Sym_ref = tcc_malloc(sizeof(Sym));
	}
    
    /* Copy the contents of the Sym stack */
    for (curr_Sym = global_stack; curr_Sym != NULL; curr_Sym = curr_Sym->prev) {
		/* See tcc.h around line 425 for descriptions of some of the fields.
		 * See also tccgen.c line 5987 to see what needs to happen for function
		 * declarations to work properly (and, in turn, line 446 for how to
		 * push a forward reference). */
		
		/* Get a pointer to the (already allocated) new Sym */
		Sym_ref = ram_tree_get_ref(rt, curr_Sym);
		new_sym = *Sym_ref;
		
		/* Copy the v value (token id). This will not be copied later, so keep
		 * things simple for now and simply strip out the extended flag. */
		new_sym->v = curr_Sym->v & ~SYM_EXTENDED;
		 
		/* Copy the assembler label */
		if (curr_Sym->asm_label != NULL) {
			int asm_label_len = strlen(curr_Sym->asm_label) + 1;
			new_sym->asm_label = tcc_malloc(asm_label_len);
			memcpy(new_sym->asm_label, curr_Sym->asm_label,
				asm_label_len);
		}
		else {
			new_sym->asm_label = NULL;
		}
		
		/* associated register. For variables, I believe that the low bits
		 * specify the register size that can hold the value while high bits
		 * indicate storage details (VT_SYM, VT_LVAL, etc). For function types,
		 * however, this gets cast as an AttributeDef and queried for function
		 * attributes; so far, I have only seen the .r field queried for the
		 * FUNC_CALL field. It matters little; copying the whole long is easy
		 * and it seems that everything works fine when it is the same for
		 * consuming contexts as for the original compilation context. */
		new_sym->r = curr_Sym->r;
		
		/* Set the type. Judging by the constants in tcc.h and code that
		 * uses this field, I'm pretty sure that the low bits in the .t field
		 * tells tcc how to load the data into a register. The high bits seem to
		 * indicate storage details, such as VT_EXTERN. Since that is not
		 * something that can be extended at runtime, I should be able to copy
		 * the value as-is and add an extern flag for variables and functions. */
		new_sym->type.t = curr_Sym->type.t;
		/* After compilation, functions and global variables point to hard
		 * locations in memory. Consuming contexts should think of these as
		 * having external storage, which is reflected in the VT_EXTERN bit of
		 * the type.t field. */
		int btype = curr_Sym->type.t & VT_BTYPE;
		if (btype == VT_FUNC
			|| new_sym->r & (VT_SYM | VT_LVAL | VT_CONST) /* global variables ?? */
		) new_sym->type.t |= VT_EXTERN;
		
		/* The type.ref field contains something useful only if the basic type
		 * is a pointer, struct, or function. See code from tccgen's
		 * compare_types for details. */
		if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
			if (_type_ref_is_not_Sym(new_sym)) {
				/* If we have a static symbol, copy its pointer directly, since
				 * after relocation it is no longer a Sym pointer at all! */
				new_sym->type.ref = curr_Sym->type.ref;
			}
			else {
				/* Otherwise it's safe: get a new Sym for it. */
				new_sym->type.ref
					= get_new_symtab_pointer(curr_Sym->type.ref, rt);
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
			|| new_sym->r & (VT_SYM | VT_LVAL | VT_CONST)
		) new_sym->c = 0;
		else new_sym->c = curr_Sym->c;
		
		/* Copy the next symbol field. Labels and gotos are tracked in a
		 * separate stack, so for these Symbols we focus on next, not
		 * jnext. The next field (I seem to recall) is used in storing
		 * argument lists, so it needs to be copied for function
		 * types. I believe it can be copied anonymously. */
		new_sym->next = get_new_symtab_pointer(curr_Sym->next, rt);
		
		/* These are only needed for symbol table pushing/popping, so I
		 * should be able to safely set them to null.  However, I will
		 * set the prev_tok field to 1 to indicate that this is a
		 * non-define Sym. */
		new_sym->prev = NULL;
		new_sym->prev_tok = (void*)1;
	}
    
    /********* define stack *********/
    
    /* As with the symbol stack, allocate memory in the ram_tree for all
     * define Syms on the define stack. This way, I can be confident
     * that an unfound lookup is actually a bad thing. */
    Sym * curr_Def;
    /* XXX this should only run to define_start!!!!!! But if it doesn't
     * then we end up with a bunch of unkown define Syms. This seems
     * to arise because I am (at the moment) copying *all* TokenSyms
     * at the moment. :-( */
    for (curr_Def = define_stack; curr_Def != NULL; curr_Def = curr_Def->prev) {
		Sym_ref = ram_tree_get_ref(rt, curr_Def);
		*Sym_ref = tcc_malloc(sizeof(Sym));
	}
    
    /* Copy the define list */
    for (curr_Def = define_stack; curr_Def != NULL; curr_Def = curr_Def->prev) {
		/* See above for descriptions of some of the fields. */
		 
		/* Get a pointer to the (already allocated) new Sym */
		Sym_ref = ram_tree_get_ref(rt, curr_Def);
		new_sym = *Sym_ref;
		
		/* Convert the symbol's token index. */
		new_sym->v = curr_Def->v & ~SYM_EXTENDED;
		
		/* assembler label should be null for preprocessor stuff */
		if (curr_Def->asm_label != NULL) {
			tcc_warning("Unexpected assembler label for macro symbol");
		}
		new_sym->asm_label = NULL;
		
		/* As far as I can tell, the 'r' field is not used by
		 * preprocessor macros. Just copy it. */
		new_sym->r = curr_Def->r;
		
		/* Copy the tokenstream if it exists */
		if (curr_Def->d != NULL) {
			int * str = curr_Def->d;
			int len = tokenstream_len(str);
			new_sym->d = tcc_malloc(sizeof(int) * len);
			/* The token ids used in the original compiling context are in the
			 * same order as the final extended symbol table, so I can just copy
			 * the token stream verbatim! */
			memcpy(new_sym->d, curr_Def->d, sizeof(int) * len);
		}
		else {
			new_sym->d = NULL;
		}
		
		/* Set the type. define_push and parse_define indicate that this
		 * will be either MACRO_OBJ or MACRO_FUNC. */
		new_sym->type.t = curr_Def->type.t;
		/* Macro types should be null; issue a warning if this is not
		 * the case, just so we're aware. */
		if (curr_Def->type.ref != NULL) {
			tcc_warning("Unexpected type ref for macro symbol");
		}
		new_sym->type.ref = NULL;
		
		/* Copy the macro arguments. All macro arguments are placed
		 * on the define stack, according to the sym_push2 in
		 * parse_define from tccpp.c. We only need to update this Sym's
		 * next; *it's* next will be updated when it comes up in this
		 * loop. */
		new_sym->next
			= get_new_deftab_pointer(curr_Def->next, rt);
		
		/* These are only needed for symbol table pushing/popping and
		 * label identification. Since these Sym objects will do no
		 * such things, I should be able to safely set them to null.
		 * However, I will set the prev_tok field to 2 to indicate that
		 * this is a define Sym. */
		new_sym->prev = NULL;
		new_sym->prev_tok = (void*)2;
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
			= get_new_deftab_pointer(tok_copy->sym_define, rt);
		tok_sym->sym_label = NULL; /* Not copying labels */
		tok_sym->sym_struct
			= get_new_symtab_pointer(tok_copy->sym_struct, rt);
		tok_sym->sym_identifier
			= get_new_symtab_pointer(tok_copy->sym_identifier, rt);
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
	
	/* Iterate through all Syms in the ram tree */
    void * iterator_data = NULL;
    do {
		/* Get the Sym pointer */
		void ** data_ref = ram_tree_iterate(symtab->rt, &iterator_data);
		Sym * to_delete = (Sym *)*data_ref;
		
		if (to_delete->prev_tok == (void*)2) {
			/* If it's a define Sym, delete the token stream */
			tcc_free(to_delete->d);
		}
		else {
			/* otherwise, clear the assembler label */
			tcc_free(to_delete->asm_label);
		}
		
		/* Clear the symbol itself */
		tcc_free(to_delete);
	} while (iterator_data != NULL);
	
	/* clean up the ram_tree itself */
	ram_tree_free(symtab->rt);
	
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

/*****************************************************************************/
/*                      Extended Symbol Table Caching                        */
/*****************************************************************************/

/* A few concerns that need to be balanced:
 * 1) The deserialization code needs to allocate memory in a way that is
 *    compatible with the deallocation code.
 * 2) It seems sensible to write slow serialization code if it means I
 *    can write fast deserialization code. For example, it would be nice
 *    if I could load *ALL* Syms with a single fread.
 * 3) Loading all Syms in one shot during deserialization would break
 *    deallocation code, unless I start using different deallocation
 *    code for the different symtabs. This is easily achievable if each
 *    symtab keeps track of its type, and then if the major methods
 *    delegate by symtab type. 
 * 4) I am not quite proposing OOP, as the actual memory layout of all
 *    symtabs will be the same. At least for now.
 * 5) In that case, tcc_delete_extended_symbol_table needs to use a
 *    switch statement that delegates to type-specific code.
 */

LIBTCCAPI int tcc_set_extended_symbol(extended_symtab * symtab, const char * name, void * pointer) {
	TokenSym * ts = tcc_get_extended_tokensym(symtab, name);
	if (ts == NULL) return 0; /* failed */
	ts->hash_next = pointer;
	/* XXX working here - update sym's type.t as well??? */
	return 1; /* succeeded */
}

/* Write the total number of tokens that live on the end of this exsymtab, as
 * well as tok_start. */
int exsymtab_serialize_N_tokens_and_tok_start(extended_symtab * symtab, FILE * out_fh) {
	int to_write[2];
	to_write[0] = symtab->tokenSym_last - symtab->tokenSym_list; /* N_tokens */
	to_write[1] = symtab->tok_start;
	if (fwrite(to_write, sizeof(int), 2, out_fh) == 2) return 1;
	
	/* Failed to serialize; write a message and return failure */
	tcc_warning("Serialization failed: Unable to serialize the number of tokens and tok_start\n";
	return 0;
}

/* Get the total number of tokens that live on the end of this exsymtab
 * and allocate an exsymtab struct with enough room; read and set tok_start */
extended_symtab * exsymtab_deserialize_init(FILE * in_fh) {
	int N_tokens;
	extended_symtab * symtab;
	if (fread(&N_tokens, sizeof(int), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to get the number of tokens\n");
		return NULL;
	}
	
	/* Allocate the symtab; follows process outlined in copy_extended_symtab */
    symtab = tcc_malloc(sizeof(extended_symtab) + sizeof(void*) * (N_tokens - 1));
    if (symtab == NULL) {
		tcc_warning("Deserialization failed: Unable to allocate symtab to hold %d tokens\n", N_tokens);
		return NULL;
	}
	
	/* deserialize tok_start */
	if (fread(&(to_return->tok_start), sizeof(int), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to get tok_start\n");
		free(symtab);
		return NULL;
	}
	
	return symtab;
}

/**** sym type, which is stored in tok_prev ****/

int exsymtab_serialize_sym_type(FILE * out_fh, Sym * curr_sym) {
	char type;
	if (curr_sym->prev_tok == (void*)1) type = 1;
	else if (curr_sym->prev_tok == (void*)2) type = 2;
	else {
		tcc_warning("Serialization failed: Unknown Sym type %p for Sym "
			" %d", curr_sym->prev_tok, curr_sym->v);
		return 0;
	}
	if (fwrite(&type, sizeof(char), 1, out_fh) != 1) {
		tcc_warnings("Serialization failed: Unable to write Sym type "
			"for Sym %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_sym_type(FILE * in_fh, Sym * curr_sym, int i) {
	char type;
	if (fread(&type, sizeof(char), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to get Sym type for "
			"Sym number %d", i);
		return 0;
	}
	curr_sym->prev_tok = (void*)type;
	return 1;
}

/**** v, token id ****/

int exsymtab_serialize_v(FILE * out_fh, Sym * curr_sym) {
	if (fwrite(&curr_sym->v, sizeof(int), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write Sym's token "
			"id (v) of %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_v(FILE * in_fh, Sym * curr_sym, int i) {
	/* Read v */
	if (fread(&(curr_sym->v), sizeof(int), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to get token id for "
			"Sym number %d", i);
		return 0;
	}
	return 1;
}

/**** Associated register ****/

int exsymtab_serialize_r(FILE * out_fh, Sym * curr_sym) {
	if (fwrite(&curr_sym->r, sizeof(curr_sym->r), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write "
			"associated register for Sym %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_r(FILE * in_fh, Sym * curr_sym, int i) {
	if (fread(&(curr_sym->r), sizeof(curr_sym->r), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to read associated "
			"register for Sym number %d", i);
		return 0;
	}
	return 1;
}

/**** type.t ****/

int exsymtab_serialize_type_t(FILE * out_fh, Sym * curr_sym) {
	if (fwrite(&curr_sym->type.t, sizeof(curr_sym->type.t), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write type.t for "
			"Sym %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_r(FILE * in_fh, Sym * curr_sym, int i) {
	if (fread(&(curr_sym->r), sizeof(curr_sym->r), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to read associated "
			"register for Sym number %d", i);
		return 0;
	}
	return 1;
}

/**** next ****/

int exsymtab_serialize_next(FILE * out_fh, Sym * curr_sym, ram_tree offset_rt) {
	/* Default to serializing NULL */
	void * to_write = NULL;
	if (curr_sym->next != NULL) {
		/* compute the pointer offset if we have a non-null "next" field */
		void ** p_offset = ram_tree_get_ref(offset_rt, curr_sym->next);
		to_write = *p_offset;
	}
	
	/* Perform the write operation */
	if (fwrite(&to_write, sizeof(void *), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write 'next' pointer "
			"for Sym %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_next(FILE * in_fh, Sym * sym_list, int i) {
	Sym * curr_sym = sym_list[i];
	uintptr_t offset;
	
	/* read the offset */
	if (fread(&offset, sizeof(void*), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to read 'next' pointer "
			"for Sym number %d", i);
		return 0;
	}
	
	/* Set next field based on offset */
	if (offset == 0) curr_sym->next = NULL;
	else curr_sym->next = sym_list + offset - 1; /* note off-by-one */
	return 1;
}

/**** token stream, field d ****/

int exsymtab_serialize_token_stream(FILE * out_fh, Sym * curr_sym) {
	/* write the length of the token stream first */
	int ts_len = 0;
	if (curr_sym->d != NULL) ts_len = tokenstream_len(curr_sym->d);
	if (fwrite(&ts_len, sizeof(int), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write token stream "
			"length for Sym %d", curr_sym->v);
		return 0;
	}
	/* If nothing to write, we're done: return success */
	if (ts_len == 0) return 1;
	
	/* Otherwise, write the tokenstream verbatim, so that the token
	 * values line up with the TokenSym values. */
	if (fwrite(curr_sym->d, sizeof(int), ts_len, out_fh) == ts_len)
		return 1;
	
	/* failed */
	tcc_warning("Serialization failed: Unable to write token stream "
		"for Sym %d", curr_sym->v);
	return 0;
}

int exsymtab_deserialize_token_stream(FILE * in_fh, Sym * curr_sym, int i) {
	/* get the length of the token stream */
	int ts_len;
	if (fread(&ts_len, sizeof(int), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to read token stream "
			"length for Sym number %d", i);
		return 0;
	}
	/* If no length, just set d to NULL and return success */
	if (ts_len == 0) {
		curr_sym->d = NULL;
		return 1;
	}
	
	/* Allocate memory for the token stream */
}

/**** type.ref ****/

int exsymtab_serialize_type_ref(FILE * out_fh, Sym * curr_sym, ram_tree offset_rt) {
	/* For details, see notes under copy_extended_symtab */
	int btype = curr_Sym->type.t & VT_BTYPE;
	void * to_write;
	if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
		if (_type_ref_is_not_Sym(curr_sym)) {
			/* We have a static symbol; just write null */
			to_write = NULL;
		}
		else {
			/* write the offset */
			void ** p_offset = ram_tree_get_ref(offset_rt, curr_sym);
			to_write = *p_offset;
		}
	}

	/* Perform the write operation */
	if (fwrite(&to_write, sizeof(void *), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write type.ref for "
			"Sym %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_type_ref(FILE * in_fh, Sym * sym_list, int i) {
	/* Unlike the case for serialization, deserializing this pointer is
	 * fairly easy. If something needs external linkage, then it will be
	 * null. It'll need to be patched by tcc_set_extended_symbol. */
	Sym * curr_sym = sym_list[i];
	uintptr_t offset;
	if (fread(&offset, sizeof(void*), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to read type.ref for "
			"Sym number %d", i);
		return 0;
	}
	/* Set offset */
	if (offset == 0) curr_sym->type.ref = NULL;
	else curr_sym->type.ref = sym_list + offset - 1;
	return 1;
}

/**** field c ****/

int exsymtab_serialize_c(FILE * out_fh, Sym * curr_sym) {
	if (fwrite(&curr_sym->c, sizeof(curr_sym->c), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write "
			"field c for Sym %d", curr_sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_c(FILE * in_fh, Sym * curr_sym, int i) {
	if (fread(&(curr_sym->c), sizeof(curr_sym->c), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to read field c "
			"for Sym number %d", i);
		return 0;
	}
	return 1;
}

/**** Assembler label ****/

int exsymtab_serialize_asm_label(FILE * out_fh, Sym * curr_sym) {
	/* no label? */
	if (curr_Sym->asm_label == NULL) {
		int asm_label_len = 0;
		if (fwrite(&asm_label_len, sizeof(int), 1, out_fh) == 1)
			return 1; /* success! */
		/* failure, indicate as much */
		tcc_warning("Serialization failed: Unable to write assembler "
			"label length of zero for token %x", curr_Sym->v);
		return 0;
	}
	
	/* we have a label. Write out its length */
	int asm_label_len = strlen(curr_Sym->asm_label);
	if (fwrite(&asm_label_len, sizeof(int), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write assembler "
			"label length for token %d", curr_Sym->v);
		return 0;
	}
	if (fwrite(curr_Sym->asm_label, sizeof(char), asm_label_len, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write assembler "
			"label for token %d", curr_Sym->v);
		return 0;
	}
	return 1;
}

int exsymtab_deserialize_asm_label(FILE * in_fh, Sym * curr_sym, int i) {
	int asm_label_len;
	if (fread(&asm_label_len, sizeof(int), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to get assembler label length for Sym number %d",
			i);
		return 0;
	}
	if (asm_label_len == 0) {
		curr_sym->asm_label = NULL;
		return 1;
	}
	
	/* Allocate memory for the assembler label length */
	curr_sym->asm_label = tcc_malloc(asm_label_length);
	if (curr_sym->asm_label == NULL) {
		tcc_warning("Deserialization failed: Unable to allocate "
			"memory for assembler label for Sym number %d", i);
		return 0;
	}
	/* Read it in */
	if (fread(curr_sym->asm_label, 1, asm_label_length, in_fh) != asm_label_length) {
		tcc_warning("Deserialization failed: Unable to read "
			"assembler label for Sym number %d", i);
		tcc_free(curr_sym->asm_label);
		return 0;
	}
	
	/* Success! */
	return 1;
}

/**** Serialize/deserialize a full Sym ****/

int exsymtab_serialize_sym(FILE * out_fh, Sym * curr_sym, ram_tree * offset_rt) {
	/* Get the symbol type, which is inconspicuously stored in the
	 * hijacked data slot prev_tok; also serialize that first */
	uintptr_t sym_type = (uintptr_t)curr_sym->prev_tok;
	if (!exsymtab_serialize_sym_type(out_fh, curr_sym)) return 0;
	
	/* both define Syms (sym_type = 2) and other syms (sym_type = 1)
	 * need v, r, type.t, and next. After that, it depends on the type */
	if (!exsymtab_serialize_v(out_fh, curr_sym)) return 0;
	if (!exsymtab_serialize_r(out_fh, curr_sym)) return 0;
	if (!exsymtab_serialize_type_t(out_fh, curr_sym)) return 0;
	if (!exsymtab_serialize_next(out_fh, curr_sym, offset_rt)) return 0;
	
	if (sym_type == 2) {
		/* Define syms only need the token stream */
		if (!exsymtab_serialize_d(out_fh, curr_sym)) return 0;
	}
	else {
		/* Other syms need a few other items */
		if (!exsymtab_serialize_type_ref(out_fh, curr_sym, offset_rt))
			return 0;
		if (!exsymtab_serialize_c(out_fh, curr_sym)) return 0;
		if (!exsymtab_serialize_asm_label(out_fh, curr_sym)) return 0;
	}
	
	/* Success! */
	return 1;
}

int exsymtab_deserialize_sym(FILE * in_fh, Sym * sym_list, int i) {
	Sym * curr_sym = sym_list + i;
	
	/* set the assembler label to null in case of early return */
	curr_sym->asm_label = NULL;
	
	/* Get the symbol type */
	if (exsymtab_serialize_sym_type(out_fh, curr_sym) == 0) return 0;
	uintptr_t sym_type = (uintptr_t)curr_sym->prev_tok;
	
	/* set or deserialize the common elements of all Syms */
	curr_sym->prev = NULL;
	if (!exsymtab_deserialize_v(in_fh, curr_sym, i)) return 0;
	if (!exsymtab_deserialize_r(in_fh, curr_sym, i)) return 0;
	if (!exsymtab_deserialize_type_t(in_fh, curr_sym, i)) return 0;
	if (!exsymtab_deserialize_next(in_fh, sym_list, i)) return 0;
	
	if (sym_type == 2) {
		/* If it is a define symbol... */
		/* asm label already null */
		curr_sym->type.ref = NULL;
		if (!exsymtab_deserialize_token_stream(in_fh, curr_sym, i))
			return 0;
	}
	else {
		/* if it is not a define symbol... */
		if (!exsymtab_deserialize_type_ref(in_fh, sym_list, i)) return 0;
		if (!exsymtab_deserialize_c(in_fh, curr_sym, i)) return 0;
		if (!exsymtab_deserialize_asm_label(in_fh, curr_sym, i)) return 0;
	}
	
	/* Success! */
	return 1;
}

ram_tree * exsymtab_serialize_syms(extended_symtab * symtab, FILE * out_fh) {
	/* Count the number of Syms in the ram_tree and build a new one to
	 * map current addresses to new offsets. Offsets begin counting at
	 * 1, not zero. */
	ram_tree * offset_rt = ram_tree_new();
	uintptr_t N_Syms = 0;
	/* Iterate through all Syms in the ram tree */
	void * iterator_data = NULL;
	do {
		N_Syms++;
		/* Get the Sym pointer */
		void ** old_ref = ram_tree_iterate(symtab->rt, &iterator_data);
		Sym * to_count = (Sym *)*data_ref;
		/* Get and set the data slot for the mapping. Note that I do
		 * not allocate any memory for this, I merely treat the void*
		 * as an integer via uintptr_t. */
		void ** new_ref = ram_tree_get_ref(offset_rt, to_count);
		*new_ref = (void*)N_Syms; /* not 1-offset, not 0-offset */
	} while (iterator_data != NULL);
	
	/* We now know the number of Syms that will be written out, and we
	 * have a mapping from current address to serialized offset. */
	int N_Syms_i = N_Syms;
	if (fwrite(&N_syms_i, sizeof(int), 1, out_fh) != 1) {
		tcc_warning("Serialization failed: Unable to write number of Syms");
		goto FAIL;
	}
	
	/* Write out the contents of each Sym in the order set by the original
	 * ram_tree iterator. */
	iterator_data = NULL;
	do {
		/* Get the Sym pointer */
		void ** old_ref = ram_tree_iterate(symtab->rt, &iterator_data);
		Sym * to_serialize = (Sym *)*data_ref;
		if (serialize_sym(out_fh, to_serialize, offset_rt) == 0)
			goto FAIL;
	} while (iterator_data != NULL);
	
	/* All done, return the offset ram_tree */
	return offset_rt;
	
	/* In case of failure, clean up the offset ram tree */
FAIL:
	ram_tree_free(offset_rt);
	return NULL;
}

int exsymtab_deserialize_syms(extended_symtab * symtab, FILE * in_fh) {
	/* Get the number of syms in sym_list */
	int N_syms;
	if (fread(&N_syms, sizeof(int), 1, in_fh) != 1) {
		tcc_warning("Deserialization failed: Unable to get number of Syms");
		return 0;
	}
	
	/* Allocate the sym_list array. */
	symtab->sym_list = tcc_malloc(sizeof(Sym) * N_Syms);
	if (symtab->sym_list == NULL) {
		tcc_warning("Deserialization failed: Unable to allocate array to hold %d Syms"
			N_syms);
		return 0;
	}
	
	/* Load the Sym data, converting pointer offsets to addresses. */
	for (int i = 0; i < N_syms; i++) {
		if (deserialize_sym(in_fh, symtab->sym_list, i) == 0) {
			/* failed, deallocate the array and return failure */
			for(; i >= 0; i--) {
				tcc_free(symtab->sym_list[i].asm_label);
			}
			tcc_free(symtab->sym_list);
			symtab->sym_list = NULL;
			return 0;
		}
	}
	
	return 1;
}

LIBTCCAPI int tcc_serialize_extended_symtab(extended_symtab * symtab, const char * output_filename) {
    /* Do nothing if we have an empty symtab. */
    if (NULL == symtab) return;
	
    /* Open the file for writing */
    FILE * out_fh = fopen(output_filename, "wb");
    if (!out_fh) {
		tcc_warning("Serialization failed: Unable ot open extended symtab serialization file %s\n",
			output_filename);
		return 0;
	}
	
	/* Start the file with two useful integers: the number of tokens and
	 * the value of tok_start. */
	if (!exsymtab_serialize_init(symtab, out_fh)) goto FAILED;
	
	/* Serialize the Syms */
	ram_tree * offset_rt = exsymtab_serialize_syms(symtab, out_fh);
	if (offset_rt == NULL) goto FAILED;
	
	...
	
	/* All set! */
	return 1;
	
FAILED:
	fclose(out_fh);
	return 0;
}

LIBTCCAPI extended_symtab * tcc_deserialize_extended_symtab(const char * input_filename) {
    /* Open the file for writing */
    FILE * in_fh = fopen(output_filename, "rb");
    if (!in_fh) {
		tcc_warning("Deserialization failed: Unable to open extended symtab serialization file %s\n",
			output_filename);
		return NULL;
	}
	
	/* Allocate the symtab */
	extended_symtab * symtab;
	symtab = exsymtab_deserialize_init(in_fh);
	if (symtab == NULL) goto FAIL_SYMTAB;
	
	/* Allocate the c_trie. This may some day be serialized and
	 * deserialized with the rest of the data, but for now keep things
	 * simple. */
	symtab->trie = c_trie_new();
	if (symtab->trie == NULL) {
		tcc_warning("Deserialization failed: Unable to allocate new c_trie\n");
		goto FAIL_TRIE;
	}
	
	/* load the Syms */
	if (exsymtab_deserialize_syms(symtab, in_fh) == 0) goto FAIL_TRIE;
	
	...
	
	/* All set! */
	return symtab;
	
	/* And here are all the ways I might fail and their associated
	 * cleanup procedures, including fall-through. */
FAIL_TRIE:
	free(symtab);
FAIL_SYMTAB:
	fclose(in_fh);
	return NULL;
}
