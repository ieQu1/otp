#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_jail.h"
#include "global.h"
#include "error.h"

static Jail jails[MAX_JAILS];

static erts_mtx_t jailtab_lck;

void
erl_jails_init(void)
{
     erts_mtx_init(&jailtab_lck, "jailtab_lock");
}

ERTS_INLINE void
erl_lock_jailtab(Uint idx)
{
     erts_mtx_lock(&jailtab_lck);
}

ERTS_INLINE void
erl_unlock_jailtab(Uint idx)
{
     erts_mtx_unlock(&jailtab_lck);
}

ERTS_INLINE int
erl_jail_parent(Eterm pid, Eterm rcvr)
{
     Uint pdata, rdata;
     ASSERT(is_internal_pid(pid) && is_pid(rcvr));
     if(is_external_pid(rcvr))
	   return 0;
     pdata = internal_pid_data(pid);
     rdata = internal_pid_data(rcvr);
     return pdata == rdata;
}
