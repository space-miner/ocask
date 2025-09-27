#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <time.h>

CAMLprim value clock_gettime_ocaml(value unit) {
  CAMLparam1(unit); 
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
    caml_failwith("clock_gettime failed");
  }

  int64_t total_ns = (int64_t)ts.tv_sec * 1000000000L + (int64_t)ts.tv_nsec;

  CAMLreturn(caml_copy_int64(total_ns));
}
