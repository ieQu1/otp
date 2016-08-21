#ifndef __JAIL_H__
#define __JAIL_H__

#include "sys.h"

typedef Uint JailId;

#  define NO_JAIL      0
#  define INHERIT_JAIL ERTS_UINT_MAX
#  define BADPERM      BADARG

#  define BIF_RESTRICT(pid) do {   \
    if ((pid)->jail != NO_JAIL) {  \
      BIF_ERROR((pid), BADPERM);   \
    }                              \
  } while(0)


#endif
