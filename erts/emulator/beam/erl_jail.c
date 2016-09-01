#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_jail.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"

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
