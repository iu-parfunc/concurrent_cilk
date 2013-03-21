// Header file for the event library for c++
#ifndef EVENT_H
#define EVENT_H
#include <cilk/common.h>
#include <cilk/cilk.h>
#include "event_cilk.h"
// MAke sure that event system is ported correctly to c++

namespace cilk_event
{
  int cilk_event(){
  event_init();
  }
 template<typename T>
    class event_c
    {
      public:

        event_c(){
          e = event_create();
        }

        int id(){ e->self; }

        int get_dep_value(int i){ ((event_data_t *) e->deps[i])->u64; }

        int deps(){ e->ndeps; }

        uint64_t get(){ ((event_data_t *) event_wait(e->self))->u64; }

        //~event() { destroy_event(e); }

//        void create() { event_create(); }

        int add_event_dep(event_c e_add){
          return event_ctl(e->self, e_add.id(), ADD);
        }

        int remove_event_dep(event_c *e_remove){
          return event_ctl(e->self, e_remove.id(), DEL);
        }

        int fulfill(T data){ // this T might need to be a pointer
          return event_fire(e->self, (event_data_t *)data);
        }

        ivar_payload_t wait(){
          return event_wait(e->self);
        }

        ivar_payload_t wait_nodeps(){
          return event_wait_nodeps(e->self);
        }

        void destroy(){
          destroy_event(e->self);
        }

      private:
        event *e; //= event_create();
    };
}
#endif
