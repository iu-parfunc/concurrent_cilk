#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>

// Being lazy
// Using macros so that we dont slow anything down
#define READIV_T(IV, TYPE) ((TYPE)__cilkrts_ivar_read(&IV))
#define READIV(IV) (__cilkrts_ivar_read(&IV))
#define WRITEIV(IV, VAL) (__cilkrts_ivar_write(&IV,(ivar_payload_t)VAL))
#define CLEARIV(IV) (__cilkrts_ivar_clear(&IV))
// gimme some sugar...
typedef __cilkrts_ivar Ivar;

