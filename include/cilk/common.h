/*
 * Copyright (C) 2010-2011 
 * Intel Corporation
 * 
 * This file is part of the Intel Cilk Plus Library.  This library is free
 * software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 * 
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 */

#ifndef INCLUDED_CILK_COMMON
#define INCLUDED_CILK_COMMON

#ifdef __cplusplus
#   include <cassert>
#else
#   include <assert.h>
#endif

/* Prefix standard library function and type names with __STDNS in order to
 * get correct lookup in both C and C++.
 */
#ifdef __cplusplus
#   define __STDNS std::
#else
#   define __STDNS
#endif

/* CILK_EXPORT - Define export of runtime functions from shared library.
 * Should be exported only from cilkrts*.dll/cilkrts*.so
 */
#ifdef _WIN32
#   ifdef IN_CILK_RUNTIME
#       define CILK_EXPORT      __declspec(dllexport)
#       define CILK_EXPORT_DATA __declspec(dllexport)
#   else
#       define CILK_EXPORT      __declspec(dllimport)
#       define CILK_EXPORT_DATA __declspec(dllimport)
#   endif  /* IN_CILK_RUNTIME */
#else /* Unix/gcc */
#   ifdef IN_CILK_RUNTIME
#       define CILK_EXPORT      __attribute__((visibility("protected")))
#       define CILK_EXPORT_DATA __attribute__((visibility("protected")))
#   else
#       define CILK_EXPORT      /* nothing */
#       define CILK_EXPORT_DATA /* nothing */
#   endif  /* IN_CILK_RUNTIME */
#endif /* Unix/gcc */

#ifdef __cplusplus
#   define __CILKRTS_BEGIN_EXTERN_C extern "C" {
#   define __CILKRTS_END_EXTERN_C }
#else
#   define __CILKRTS_BEGIN_EXTERN_C
#   define __CILKRTS_END_EXTERN_C
#endif

#ifdef __cplusplus
#   ifdef _WIN32
#       define __CILKRTS_NOTHROW __declspec(nothrow)
#   else /* Unix/gcc */
#       define __CILKRTS_NOTHROW __attribute__((nothrow))
#   endif /* Unix/gcc */
#else
#   define __CILKRTS_NOTHROW /* nothing */
#endif /* __cplusplus */

#ifdef _WIN32
#   define CILK_ALIGNAS(n) __declspec(align(n))
#else /* Unix/gcc */
#   define CILK_ALIGNAS(n) __attribute__((aligned(n)))
#endif /* Unix/gcc */

/* CILK_API: Called explicitly by the programmer.
 * CILK_ABI: Called by compiler-generated code.
 * CILK_ABI_THROWS: An ABI function that may throw an exception
 *
 * Even when these are the same definitions, they should be separate macros so
 * that they can be easily found in the code.
 */

#ifdef _WIN32
#   define CILK_API(RET_TYPE) CILK_EXPORT RET_TYPE __CILKRTS_NOTHROW __cdecl
#   define CILK_ABI(RET_TYPE) CILK_EXPORT RET_TYPE __CILKRTS_NOTHROW __cdecl
#   define CILK_ABI_THROWS(RET_TYPE) CILK_EXPORT RET_TYPE __cdecl
#else
#   define CILK_API(RET_TYPE) CILK_EXPORT RET_TYPE __CILKRTS_NOTHROW
#   define CILK_ABI(RET_TYPE) CILK_EXPORT RET_TYPE __CILKRTS_NOTHROW
#   define CILK_ABI_THROWS(RET_TYPE) CILK_EXPORT RET_TYPE
#endif

/* __CILKRTS_ASSERT should be defined for debugging only, otherwise it
 * interferes with vectorization.  Since NDEBUG is not reliable (it must be
 * set by the user), we must use a platform-specific detection of debug mode.
 */
#if defined(_WIN32) && defined(_DEBUG)
    /* Windows debug */
#   define __CILKRTS_ASSERT(e) assert(e)
#elif (! defined(_WIN32)) && ! defined(__OPTIMIZE__)
    /* Unix non-optimized */
#   define __CILKRTS_ASSERT(e) assert(e)
#elif defined __cplusplus
    /* C++ non-debug */
#   define __CILKRTS_ASSERT(e) static_cast<void>(0)
#else
    /* C non-debug */
#   define __CILKRTS_ASSERT(e) ((void) 0)
#endif

// Inlining is always available, but not always the same way.
#ifdef __cpluspus
    // C++
#   define __CILKRTS_INLINE inline
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
    // C99
#   define __CILKRTS_INLINE static inline
#elif defined(_MSC_VER)
    // C89 on Windows
#   define __CILKRTS_INLINE __inline
#else
    // C89 on Linux
#   define __CILKRTS_INLINE __inline__
#endif

// Functions marked as CILK_EXPORT_AND_INLINE have both
// inline versions defined in the Cilk API, as well as
// non-inlined versions that are exported (for
// compatibility with previous versions that did not
// inline the functions).
#ifdef COMPILING_CILK_API_FUNCTIONS
#   define CILK_EXPORT_AND_INLINE  CILK_EXPORT
#else
#   define CILK_EXPORT_AND_INLINE  __CILKRTS_INLINE
#endif

// Try to determine if compiler supports rvalue references.
#if defined(__cplusplus) && !defined(__CILKRTS_RVALUE_REFERENCES)
#   if __cplusplus >= 201103L // C++11
#       define __CILKRTS_RVALUE_REFERENCES 1
#   elif defined(__GXX_EXPERIMENTAL_CXX0X__)
#       define __CILKRTS_RVALUE_REFERENCES 1
#   elif __cplusplus >= 199711L && __cplusplus < 201103L
        // Compiler recognizes a language version prior to C++11
#   elif __INTEL_COMPILER == 1200 && defined(__STDC_HOSTED__)
        // Intel compiler version 12.0
        // __cplusplus has a non-standard definition.  In the absence of a
        // proper definition, look for the C++0x macro, __STDC_HOSTED__.
#       define __CILKRTS_RVALUE_REFERENCES 1
#   elif __INTEL_COMPILER > 1200 && defined(CHAR16T)
        // Intel compiler version >= 12.1
        // __cplusplus has a non-standard definition.  In the absence of a
        // proper definition, look for the Intel macro, CHAR16T
#       define __CILKRTS_RVALUE_REFERENCES 1
#   endif
#endif

/*
 * Include stdint.h to define the standard integer types.
 *
 * Unfortunately Microsoft doesn't provide stdint.h until Visual Studio 2010,
 * so use our own definitions until those are available
 */

#if ! defined(_MSC_VER) || (_MSC_VER >= 1600)
#   include <stdint.h>
#else
#   ifndef __MS_STDINT_TYPES_DEFINED__
#       define __MS_STDINT_TYPES_DEFINED__
        typedef signed char int8_t;
        typedef short int16_t;
        typedef int int32_t;
        typedef __int64 int64_t;

        typedef unsigned char uint8_t;
        typedef unsigned short uint16_t;
        typedef unsigned int uint32_t;
        typedef unsigned __int64 uint64_t;
#   endif  /* __MS_STDINT_TYPES_DEFINED__ */
#endif  /* ! defined(_MSC_VER) || (_MSC_VER >= 1600) */

/**
 * @brief Application Binary Interface version of the Cilk runtime library.
 *
 * The ABI version is determined by the compiler used.  An object file
 * compiled with a higher ABI version is not compatible with a library that is
 * compiled with a lower ABI version.  An object file compiled with a lower
 * ABI version, however, can be used with a library compiled with a higher ABI
 * version unless otherwise stated.
 */
#ifndef __CILKRTS_ABI_VERSION
#   ifdef IN_CILK_RUNTIME
#       define __CILKRTS_ABI_VERSION 1
#   elif __INTEL_COMPILER > 1200
        // Intel compiler version >= 12.1
#       define __CILKRTS_ABI_VERSION 1
#   else
        // Compiler does not support ABI version 1
        // (Non-Intel compiler or Intel compiler prior to version 12.1).
#       define __CILKRTS_ABI_VERSION 0
#   endif
#endif

// These structs are exported because the inlining of
// the internal version of API methods require a worker
// structure as parameter. 
__CILKRTS_BEGIN_EXTERN_C
    struct __cilkrts_worker;
    typedef struct __cilkrts_worker __cilkrts_worker;
    typedef struct __cilkrts_worker *__cilkrts_worker_ptr;
    CILK_ABI(__cilkrts_worker_ptr) __cilkrts_get_tls_worker(void);
__CILKRTS_END_EXTERN_C

                                   
#if __CILKRTS_ABI_VERSION >= 1
// Pedigree API is available only for compilers that use ABI version >= 1.

/** Pedigree information kept in the worker and stack frame */
typedef struct __cilkrts_pedigree
{
    /** Rank at start of spawn helper. Saved rank for spawning functions */
    uint64_t rank;
                                         
    /** Link to next in chain */
    const struct __cilkrts_pedigree *parent;
} __cilkrts_pedigree;

#endif // __CILKRTS_ABI_VERSION >= 1

// RRN: Turning IVars on unconditionally for now:
//
// To turn on IVars define CILK_IVARS to be one of the following constants.  Leave
// CILK_IVARS undefined to disable the feature.
//
// During the experimental phase there will be three variants.
// The first is a simple reference implementation.  IVar blocking merely spins.  It works
// only for serially executable (write before read) Cilk/IVar programs.
#define CILK_IVARS_BUSYWAIT_VARIANT 1 
// The second variant uses a separate stack for each blocked computation.  It works for
// non-serializable Cilk/IVar programs.
#define CILK_IVARS_NORMAL_VARIANT   2
// For development/benchmarking purposes let us go ahead and implement the same
// API for Concurrent Cilk using full blown system threads.
#define CILK_IVARS_PTHREAD_VARIANT  3
// Finally, this [UNFINISHED] variant will extend the second with a cheaper, pedigree-restricted form of
// stealing that reuses the current stack.  It works only for programs supporting serial
// elision ("serializable IVar programs").
#define CILK_IVARS_PEDIGREE_VARIANT 4

// what queue we want to use:
#define LOCKFREE_QUEUE_VERSION 1
//#define B_QUEUE_VERSION 1
//#define CACHE_AWARE_QUEUE 1

// Set the default:
#ifndef CILK_IVARS
#define CILK_IVARS CILK_IVARS_NORMAL_VARIANT
#define CILK_IVAR_FOLLOW_FORWARDING 1
//#define CILK_IVARS CILK_IVARS_BUSYWAIT_VARIANT
//#define CILK_IVARS CILK_IVARS_PTHREAD_VARIANT
#endif

// turn on stack and worker caching
// if a global cache isn't defined, this will use a per worker cache
#ifndef CILK_IVARS_CACHING 
#define CILK_IVARS_CACHING  1
#endif

// use a global stack and worker cache
//#ifndef CILK_IVARS_GLOBAL_CACHE
//#define CILK_IVARS_GLOBAL_CACHE  1
//#endif


//if you want to turn debugging for ivars on, use -D DEBUG_CILK_IVARS when compiling your program
//#ifdef DEBUG_CILK_IVARS
// Set IVAR_DBG to [0-5] to control debug verbosity.
#ifndef IVAR_DBG 
#define IVAR_DBG (cilkg_get_global_state())->dbg_level
//#endif
#endif

#endif /* INCLUDED_CILK_COMMON */
