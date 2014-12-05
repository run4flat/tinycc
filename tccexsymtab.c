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

/* for an excellent paper on the topic, see http://lampwww.epfl.ch/papers/triesearches.pdf.gz
 * This is a slightly simplified implementation, but it may get more complete if
 * benchmarks warrant it. */

/* How many buckets? in 0-9, A-Z, a-z, and _, we have 10 + 26 + 26 + 1 = 63.
 * That fits nicely into an unsigned long long. */

/* Define the compressed trie used for extended table lookups. */
struct compressed_trie {
	unsigned long long filled_bits;
	void * data;
	struct compressed_trie * children[1];
};
typedef struct compressed_trie c_trie;

/* Returns a c_trie with a single allocated child. This should serve as the head
 * of the data structure. It does not store any data itself. The single child is
 * actually the first node in the trie. */
c_trie * c_trie_new() {
	c_trie * new_trie = tcc_malloc(sizeof(c_trie));
	/* set to 1 so that c_trie_free works recursively without special treatment
	 * of head node */
	new_trie->filled_bits = 1;
	new_trie->data = 0;
	return new_trie;
}

/* See http://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
 * with the explanation that begins, "I think the fastest way-without using
 * lookup tables and popcount-is the following..." */
unsigned char _c_trie_popcount (unsigned long long v) {
	/* put count of each 2 bits into those 2 bits */
	v = v - ((v >> 1) & 0x5555555555555555);
	/* put count of each 4 bits into those 4 bits */
	v = (v & 0x3333333333333333) + ((v >> 2) & 0x3333333333333333);
	/* put count of each 8 bits into those 8 bits */
	v = (v & 0x1111111111111111) + ((v >> 4) & 0x1111111111111111);
	/* sum up those bits */
	return ((v + ((v >> 8) & 0xF000F000F000F)) * 0x1000100010001) >> 48;
}

/* Recursively free all children, then free self */
void c_trie_free(c_trie * curr) {
	unsigned char N_children = _c_trie_popcount(curr->filled_bits);
	unsigned char i;
	for (i = 0; i < N_children; i++) c_trie_free(curr->children[i]);
	tcc_free(curr);
}

unsigned char _c_trie_bit_offset_for_char (char c) {
	/* Find the bit offset. This requires some algebra in order to fit the
	 * allowable ASCII characters into just 64 bits. Since most characters in
	 * the trie are going to be lowercase, I handle their calculation first.
	 * I then move on to uppercase (after handling the underscore to keep the
	 * logic simple), and lastly to digits. */
	if (c >= 'a') return c - 'a';
	if (c == '_') return 62;
	if (c >= 'A') return c - 'A' + 26;
	return c - '0' + 52;
}

/* Find the slot where this character lives and return a pointer to the child. A
 * null pointer means we couldn't find it. */
c_trie ** _c_trie_find_child (c_trie * current, char * string) {
	unsigned char bit_offset = _c_trie_bit_offset_for_char(*string);
	
	/* Create the bit associated with the character */
	unsigned long long curr_bit = 1 << bit_offset;
	
	/* See if it is in our set of buckets */
	if ((current->filled_bits & curr_bit) == 0) return NULL;
	
	/* Find the compressed offset of the child associated with this character
	 * and return it. */
	unsigned long long mask_to_popcount = 0xFFFFFFFFFFFFFFFF >> (64 - bit_offset);
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
	/* At this point we have found the node that supposedly contains our data */
	return (*child_p)->data;
}

void c_trie_add_data (c_trie * head, char * string, void * data) {
	/* This is called with the head node, so we must go to the first child in
	 * order to begin the actual search. */
	c_trie ** curr_p = head->children;
	c_trie ** child_p;
	
	while(*string > 0) {
		child_p = _c_trie_find_child(*curr_p, string);
		if (child_p == NULL) {
			/* No child for this character, so create a new child. */
			c_trie * new_child = tcc_mallocz(sizeof(c_trie) - sizeof(c_trie*));
			
			/* Create a new curr with additional storage for this child */
			c_trie * old = *curr_p;
			unsigned char N_children = _c_trie_popcount(old->filled_bits);
			c_trie * new = tcc_malloc(sizeof(c_trie) + N_children * sizeof(c_trie*));
			N_children++;
			
			/* Set the new filled bits */
			unsigned long long new_bit = 1 << _c_trie_bit_offset_for_char(*string);
			new->filled_bits = old->filled_bits | new_bit;
			
			/* Copy the old children, interleavinig the new child */
			unsigned char i = 0;
			unsigned char old_i = 0;
			unsigned long long curr_bit = 1;
			while(i < N_children) {
				if (old->filled_bits & curr_bit) {
					new->children[i] = old->children[old_i];
					i++;
					old_i++;
				}
				else if (new_bit == curr_bit) {
					new->children[i] = new_child;
					child_p = &(new->children[i]); /* update child_p */
					i++;
				}
				curr_bit <<= 1;
			}
			
			tcc_free(old); /* free old node */
			*curr_p = new; /* update parent's list to point to the new node */
		}
		
		/* next character, advance through the trie */
		string++;
		curr_p = child_p;
	}
	
	/* Finally, the node pointed to by curr_p has a data slot that we want to
	 * update. */
	(*curr_p)->data = data;
}
