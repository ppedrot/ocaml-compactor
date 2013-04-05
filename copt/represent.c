/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <memory.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include "represent.h"

#define ENTRIES_PER_TRAIL_BLOCK  1025

/* Color handling */

#define Caml_white (0 << 8)
#define Caml_gray  (1 << 8)
#define Caml_blue  (2 << 8)
#define Caml_black (3 << 8)

#define Bluehd_hd(hd)  (((hd)  & ~Caml_black)  | Caml_blue)

#define Colornum_hd(hd) ((color_t) (((hd) >> 8) & 3))
#define Coloredhd_hd(hd,colnum) (((hd) & ~Caml_black) | ((colnum) << 8))

static uintnat obj_counter;  /* Number of objects emitted so far */

static void extern_out_of_memory(void);
// static void extern_invalid_argument(char *msg);

struct trail_entry {
  value obj;    /* address of object + initial color in low 2 bits */
  value field0; /* initial contents of field 0 */
};

struct trail_block {
  struct trail_block * previous;
  struct trail_entry entries[ENTRIES_PER_TRAIL_BLOCK];
};

static struct trail_block extern_trail_first;
static struct trail_block * extern_trail_block;
static struct trail_entry * extern_trail_cur, * extern_trail_limit;

/* Initialize the trail */

static void init_extern_trail(void)
{
  extern_trail_block = &extern_trail_first;
  extern_trail_cur = extern_trail_block->entries;
  extern_trail_limit = extern_trail_block->entries + ENTRIES_PER_TRAIL_BLOCK;
}

/* Replay the trail, undoing the in-place modifications
   performed on objects */

static void extern_replay_trail(void)
{
  struct trail_block * blk, * prevblk;
  struct trail_entry * ent, * lim;

  blk = extern_trail_block;
  lim = extern_trail_cur;
  while (1) {
    for (ent = blk->entries; ent < lim; ent++) {
      value obj = ent->obj;
      // Unset the blue color for the current object
      color_t colornum = obj & 3;
      obj = obj & ~3;
      Hd_val(obj) = Coloredhd_hd(Hd_val(obj), colornum);
      // Restore the first field
      Field(obj, 0) = ent->field0;
    }
    if (blk == &extern_trail_first) break;
    // Continue on previous blocks
    prevblk = blk->previous;
    free(blk);
    blk = prevblk;
    lim = &(blk->entries[ENTRIES_PER_TRAIL_BLOCK]);
  }
  /* Protect against a second call to extern_replay_trail */
  extern_trail_block = &extern_trail_first;
  extern_trail_cur = extern_trail_block->entries;
}

/* Set forwarding pointer on an object and add corresponding entry
   to the trail. */

static void extern_record_location(value obj)
{
  header_t hdr;

  if (extern_trail_cur == extern_trail_limit) {
    struct trail_block * new_block = malloc(sizeof(struct trail_block));
    if (new_block == NULL) extern_out_of_memory();
    new_block->previous = extern_trail_block;
    extern_trail_block = new_block;
    extern_trail_cur = extern_trail_block->entries;
    extern_trail_limit = extern_trail_block->entries + ENTRIES_PER_TRAIL_BLOCK;
  }
  // Set the blue color on current object header
  hdr = Hd_val(obj);
  extern_trail_cur->obj = obj | Colornum_hd(hdr);
  // Save the object first field
  extern_trail_cur->field0 = Field(obj, 0);
  extern_trail_cur++;
  Hd_val(obj) = Bluehd_hd(hdr);
  // Set the current counter in first field
  Field(obj, 0) = (value) obj_counter;
  obj_counter++;
}

/* Exception raising, with cleanup */

static void extern_out_of_memory(void)
{
  extern_replay_trail();
//   free_extern_output();
  caml_raise_out_of_memory();
}

/* Marshal the given value in the output buffer */

static void extern_rec(value v)
{
 tailcall:
  if (Is_long(v)) {
    intnat n = Long_val(v);
    return;
  }
  else {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);

    if (tag == Forward_tag) {
      value f = Forward_val (v);
      if (Is_block (f)
          && (Tag_val (f) == Closure_tag || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag)){
        /* Do not short-circuit the pointer. */
      }else{
        v = f;
        goto tailcall;
      }
    }
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      return;
    }
    /* Check if already seen */
    if (Color_hd(hd) == Caml_blue) {
      uintnat d = obj_counter - (uintnat) Field(v, 0);
      return;
    }

    /* Output the contents of the object */
    switch(tag) {
    case String_tag: {
      mlsize_t len = caml_string_length(v);
      extern_record_location(v);
      break;
    }
    case Double_tag: {
//       if (sizeof(double) != 8)
//         extern_invalid_argument("output_value: non-standard floats");
      extern_record_location(v);
      break;
    }
    case Double_array_tag: {
      mlsize_t nfloats;
//       if (sizeof(double) != 8)
//         extern_invalid_argument("output_value: non-standard floats");
      nfloats = Wosize_val(v) / Double_wosize;
      extern_record_location(v);
      break;
    }
    case Abstract_tag:
//       extern_invalid_argument("output_value: abstract value (Abstract)");
      break;
//     case Infix_tag:
//       writecode32(CODE_INFIXPOINTER, Infix_offset_hd(hd));
//       extern_rec(v - Infix_offset_hd(hd));
//       break;
    case Custom_tag: {
      extern_record_location(v);
      break;
    }
    default: {
      value field0;
      mlsize_t i;
      field0 = Field(v, 0);
      extern_record_location(v);
      if (sz == 1) {
        v = field0;
      } else {
        extern_rec(field0);
        for (i = 1; i < sz - 1; i++) extern_rec(Field(v, i));
        v = Field(v, i);
      }
      goto tailcall;
    }
    }
  }
}
