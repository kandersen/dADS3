#include <time.h>
#include <stdint.h>

void begin_time           (void);
void end_time             (void);
int64_t get_elapsed      (void);
int64_t measure_function (void (*f) (void));
