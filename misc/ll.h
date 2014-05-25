#ifndef _LL_H
#define _LL_H

/* ------------------------------------------------------------------------ */
/*  LL_T -- Simple linked-list structure.                                   */
/*                                                                          */
/*  We use this struct as the first element of each structure that we want  */
/*  to put into a singly-linked list.  We do this, because we build each    */
/*  list in reverse, and then fix the order afterwards.  Since we want to   */
/*  use the same reverse function for all lists, we use a generic list      */
/*  struct and C's guarantee that a pointer to the first element of a       */
/*  struct is equal to a pointer to the struct.                             */
/* ------------------------------------------------------------------------ */
typedef struct ll_t { struct ll_t *next; } ll_t;

/* ======================================================================== */
/*  LL_REVERSE    -- Reverses a LL_T linked list.                           */
/*  LL_INSERT     -- Inserts an element at the head of a linked list.       */
/*  LL_CONCAT     -- Concatenate one list onto another.                     */
/*  LL_FREE       -- Frees a linked list.                                   */
/*  LL_ACTON      -- Performs an action on each element of a linked list.   */
/* ======================================================================== */
ll_t *ll_reverse(ll_t *RESTRICT l);
ll_t *ll_concat (ll_t *RESTRICT head, ll_t *RESTRICT const list);
ll_t *ll_insert (ll_t *const RESTRICT head, ll_t *const RESTRICT elem);
void  ll_acton  (ll_t *RESTRICT head, void (act)(ll_t *, void *), void *opq);
void  ll_free   (ll_t *list);

#define LL_REVERSE(h,t)  ((h) = (t*)ll_reverse(&((h)->l)))
#define LL_CONCAT(h,n,t) ((h) = (t*)ll_concat (&((h)->l),&((n)->l)))

#endif /* LL_H_ */
