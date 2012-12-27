/*  ivar.h                  -*-C++-*-
 *
 *  Copyright 2009-2011 Intel Corporation.  All Rights Reserved.
 *
 *  The source code contained or described herein and all documents related
 *  to the source code ("Material") are owned by Intel Corporation or its
 *  suppliers or licensors.  Title to the Material remains with Intel
 *  Corporation or its suppliers and licensors.  The Material is protected
 *  by worldwide copyright laws and treaty provisions.  No part of the
 *  Material may be used, copied, reproduced, modified, published, uploaded,
 *  posted, transmitted, distributed, or disclosed in any way without
 *  Intel's prior express written permission.
 *
 *  No license under any patent, copyright, trade secret or other
 *  intellectual property right is granted to or conferred upon you by
 *  disclosure or delivery of the Materials, either expressly, by
 *  implication, inducement, estoppel or otherwise.  Any license under such
 *  intellectual property rights must be express and approved by Intel in
 *  writing.
 *
 */

#ifndef CILK_IVAR_H_INCLUDED
#define CILK_IVAR_H_INCLUDED

#include <cilk/common.h>
#include <cilk/cilk_api.h>


// RRN: First version.  A very simple "boxed" ivar approach which uses a malloc+pointer
// for the contents of each IVar.  This should be replaced in the future with an unboxed
// version.

// TODO -- FUTURE OPTIONS, pick one:
// 
//   Option 1:  Keep the pure C implementation.  Use template specialization to achieve
//              an unboxed implementation for scalars less than one word in size.
//              This will preclude unboxed structs/tuples...
//              NOTE: I took a first step towards this approach below.
// 
//   Option 2:  Eliminate the __cilkrts_ivar C implementation and do a C++-only version.
//


template<typename T>
class ivar
{
  public:
    ivar() { clear(); }

    ~ivar() { if(tmpptr) delete tmpptr; }

    // Copying creates a NEW empty ivar.
    // It could also clone the contents of the ivar if it is full... but that seems messy.
    ivar<T>(const ivar<T>& d) { clear(); }

    void put(const T& val) 
    {
      // No error checking!  This will only happen once, and if it happens more than
      // once the underlying __cilkrts_ivar_write is expected to catch it.

      // Allocate space for a copy:
      tmpptr = new T();
      *tmpptr = val; // Copy it.
      // Store the pointer in the ivar:
      __cilkrts_ivar_write(&iv, (ivar_payload_t)tmpptr);
    }

    const T& get() 
    {
      tmpptr = (T*)__cilkrts_ivar_read(&iv);
      return *tmpptr;
    }

  private:
    void clear() {
      tmpptr = NULL;
      __cilkrts_ivar_clear(&iv);
    }

    T* tmpptr; // Need a place to store things to return const references:
    __cilkrts_ivar iv;
};

//==================== Unboxing via Template Specialization ========================

#define CILK_IVAR_TEMPLATE_SPECIALIZED
#ifdef CILK_IVAR_TEMPLATE_SPECIALIZED

// TODO: What is the best way to repeat this for all scalar types that fit in a word?
// Must I make a preprocessor macro?
template<>
class ivar <int>
{
  public:

    ivar() {
      // if(IVAR_DBG) fprintf(stderr," [ivar] CREATING TEMPLATE SPECIALIZED INT IVAR\n");
      __cilkrts_ivar_clear(&iv);
    }
    //    void put(const int& val) {
    void put(const int val) {
      // Store the pointer in the ivar:
      __cilkrts_ivar_write(&iv, (ivar_payload_t)val);
    }
    //    const int& get() {
    int get() {
      int tmp = (int)__cilkrts_ivar_read(&iv);
      return tmp;
    }

    ivar<int>(const ivar<int>& d) { __cilkrts_ivar_clear(&iv); }
  private:
    __cilkrts_ivar iv;
};

// Specialize for pointer types:
// DUPLICATE CODE:
template<typename T>
class ivar <T*>
{
  public:
    ivar() {
      // if(IVAR_DBG) fprintf(stderr," [ivar] CREATING TEMPLATE SPECIALIZED POINTER IVAR\n");
      __cilkrts_ivar_clear(&iv);
    }
    void put(T* val) {
      // Store the pointer in the ivar:
      __cilkrts_ivar_write(&iv, (ivar_payload_t)val);
    }
    T* get() {
      T* tmp = (T*)__cilkrts_ivar_read(&iv);
      return tmp;
    }

    ivar<T*>(const ivar<T*>& d) { __cilkrts_ivar_clear(&iv); }
  private:
     __cilkrts_ivar iv;
};


#endif // CILK_IVAR_TEMPLATE_SPECIALIZED


#endif /* CILK_IVAR_H_INCLUDED */
