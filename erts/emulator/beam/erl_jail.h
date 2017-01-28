#ifndef __JAIL_H__
#define __JAIL_H__

/* #include "sys.h" */

typedef Uint JailId;

#  define NO_JAIL      0
#  define INHERIT_JAIL ERTS_UINT_MAX
#  define BADPERM      EXC_BADPERM

#  define BIF_RESTRICT(pid) do {   \
    if ((pid)->jail != NO_JAIL) {  \
      BIF_ERROR((pid), BADPERM);   \
    }                              \
  } while(0)

// JAILTODO: Not effective
#  define JAIL_PARENT(pid, target) \
     (erl_jail_parent((pid)->group_leader, (target)->common.id))

#  define JAIL_VALID_RECEIVER(pid, target)			\
     ((pid)->jail == NO_JAIL ||					\
      (pid)->jail == (target)->jail ||				\
      JAIL_PARENT(pid, target))

#define MAX_JAILS 1024

typedef struct {
    Eterm id;
    Uint64 total_reds;
    Uint64 max_reds;
    Uint64 total_heap;
    Uint64 max_heap;
} Jail;

int
erl_jail_parent(Eterm pid, Eterm rcvr);

void
erl_jails_init(void);

void
erl_lock_jailtab(JailId jail);

void
erl_unlock_jailtab(JailId jail);

#endif
