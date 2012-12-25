
/*

g++  -c -I/nfs/site/home/rrnewton/tbb/2.2/include -I../../distro/include -I. -O  -DDEBUG_KERNEL_ORIENTED=0  -o FindPrimes_NewApi.o FindPrimes_NewApi.cpp
g++ -o FindPrimes_NewApi FindPrimes_NewApi.o -L/nfs/site/home/rrnewton/tbb/2.2/intel64/cc4.1.0_libc2.4_kernel2.6.16.21/lib -L../../distro/lib/intel64 -lcnc -ltbb_debug -ltbbmalloc_debug 
g++  -c -I/nfs/site/home/rrnewton/tbb/2.2/include -I../../distro/include -I. -O  -DDEBUG_KERNEL_ORIENTED=0  -o FindPrimes_range_NewApi.o FindPrimes_range_NewApi.cpp
g++ -o FindPrimes_range_NewApi FindPrimes_range_NewApi.o -L/nfs/site/home/rrnewton/tbb/2.2/intel64/cc4.1.0_libc2.4_kernel2.6.16.21/lib -L../../distro/lib/intel64 -lcnc -ltbb_debug -ltbbmalloc_debug 
g++ -D_DIST_  -c -I/nfs/site/home/rrnewton/tbb/2.2/include -I../../distro/include -I. -O  -DDEBUG_KERNEL_ORIENTED=0  -o distFindPrimes_NewApi.o FindPrimes_NewApi.cpp
g++ -o distFindPrimes_NewApi distFindPrimes_NewApi.o -L/nfs/site/home/rrnewton/tbb/2.2/intel64/cc4.1.0_libc2.4_kernel2.6.16.21/lib -L../../distro/lib/intel64 -lcnc -ltbb_debug -ltbbmalloc_debug 
rm FindPrimes_range_NewApi.o FindPrimes_NewApi.o

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tbb/tick_count.h>

#include <cnc/cnc.h>

#include "cycle.h"

struct my_context;

struct UnblockStep
{
    int execute( int n, my_context & c ) const;
};

struct BlockStep
{
    int execute( int n, my_context & c ) const;
};

struct my_context : public CnC::context< my_context >
{
    CnC::tag_collection<int> m_tags;
    CnC::tag_collection<int> ub_tags;

    CnC::item_collection<int,int> m_dat;

    CnC::step_collection<BlockStep> m_findPrimesStepC;
    CnC::step_collection<UnblockStep> m_ubstep;

    my_context() 
        : CnC::context< my_context >(),
          m_findPrimesStepC( this, "BlockStep" ),
          m_ubstep         ( this, "UnblockStep" ),
          m_tags( this ),
          ub_tags( this ),
          m_dat( this )
    {
        prescribe( m_tags, m_findPrimesStepC );
        prescribe( ub_tags, m_ubstep );
        produce ( m_findPrimesStepC, m_dat );
        produce ( this->m_env, m_tags );
    }
};


ticks before, middle, after;

int first_time = 1;

int BlockStep::execute( int n, my_context & c ) const
{
    // Manual memoize:
    if (first_time) {
       c.ub_tags.put(n);
       before = getticks();
    } else 
        middle = getticks();
    first_time = 0;
    int x;     
    // This will definitely retry!
    c.m_dat.get(n, x);
    after = getticks();    

    printf("Aborted and retried via cnc: %lf\n", elapsed(after,before));

    printf("Middle to end : %lf\n", elapsed(after,middle));
    printf("Start to midlde : %lf\n", elapsed(middle, before));
    
    return CnC::CNC_Success;
}


int UnblockStep::execute( int n, my_context & c ) const
{
    c.m_dat.put(n, n);
    return CnC::CNC_Success;
}

int main(int argc, char* argv[])
{
    //    CnC::dist_cnc_init< my_context > dc_init;

    bool verbose = false;
    int n = 0;
    my_context c;

    printf("Prescribing one step.\n");
    c.m_tags.put(999);
    c.wait();
    //    tbb::tick_count t1 = tbb::tick_count::now();
}

