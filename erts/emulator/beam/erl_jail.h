#ifndef __JAIL_H__
#define __JAIL_H__

/* #include "sys.h" */

typedef Uint JailId;

#  define NO_JAIL      0
#  define INHERIT_JAIL ERTS_UINT_MAX
#  define BADPERM      BADARG

#  define BIF_RESTRICT(pid) do {   \
    if ((pid)->jail != NO_JAIL) {  \
      BIF_ERROR((pid), BADPERM);   \
    }                              \
  } while(0)

#define MAX_JAILS 1024

typedef struct {
    Eterm id;
    Uint64 total_reds;
    Uint64 max_reds;
    Uint64 total_heap;
    Uint64 max_heap;  
} jail;

void
erl_jails_init(void);

void
erl_lock_jailtab(Uint idx);

void
erl_unlock_jailtab(Uint idx);

#endif
