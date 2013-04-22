FILE(REMOVE_RECURSE
  "CMakeFiles/cilk-headers"
  "/opt/cilkplus/include/cilk/cilk.h"
  "/opt/cilkplus/include/cilk/cilk_api.h"
  "/opt/cilkplus/include/cilk/cilk_api_linux.h"
  "/opt/cilkplus/include/cilk/cilk_stub.h"
  "/opt/cilkplus/include/cilk/cilk_undocumented.h"
  "/opt/cilkplus/include/cilk/common.h"
  "/opt/cilkplus/include/cilk/holder.h"
  "/opt/cilkplus/include/cilk/hyperobject_base.h"
  "/opt/cilkplus/include/cilk/reducer.h"
  "/opt/cilkplus/include/cilk/reducer_file.h"
  "/opt/cilkplus/include/cilk/reducer_list.h"
  "/opt/cilkplus/include/cilk/reducer_max.h"
  "/opt/cilkplus/include/cilk/reducer_min.h"
  "/opt/cilkplus/include/cilk/reducer_opadd.h"
  "/opt/cilkplus/include/cilk/reducer_opand.h"
  "/opt/cilkplus/include/cilk/reducer_opor.h"
  "/opt/cilkplus/include/cilk/reducer_opxor.h"
  "/opt/cilkplus/include/cilk/reducer_ostream.h"
  "/opt/cilkplus/include/cilk/reducer_string.h"
  "/opt/cilkplus/include/cilk/ivar.h"
  "/opt/cilkplus/include/cilk/concurrent_cilk.h"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/cilk-headers.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
