#ifndef DADS3_INTOPTION_H
#define DADS3_INTOPTION_H

#include <stdint.h>

typedef uint32_t int_option;

int 
is_none (int_option);

int
is_some (int_option);

int_option
none (void);

int_option
some (int);


#endif
