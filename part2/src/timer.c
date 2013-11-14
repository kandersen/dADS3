#include "timer.h"

#ifdef __linux
typedef struct timespec _time_t;
#else
typedef clock_t _time_t;
#endif

_time_t _start_time, _end_time;


void 
_take_time (_time_t* res)
{
#ifdef __linux
     clock_gettime(CLOCK_PROCESS_CPUTIME_ID, res);
#else
     *res = clock();
#endif
}

void begin_time () { _take_time(&_start_time); }
void end_time () { _take_time(&_end_time); }

int64_t
get_elapsed ()
{
#ifdef __linux
     int64_t t = 0;
     
     t += _end_time.tv_sec*1000000000;
     t -= _start_time.tv_sec*1000000000;
     t += _end_time.tv_nsec;
     t -= _start_time.tv_nsec;
     return t;
#else
     return (uint64_t) _end_time - _start_time;
#endif
}

int64_t
measure_function (void (*f) (void))
{
     begin_time();
     f();
     end_time();
     return get_elapsed();
}
