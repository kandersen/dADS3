#include "int_option.h"

is_none (int_option i)
{
  return (i >> 24) & 1;
}

int
is_some (int_option i)
{
  return !is_none(i);

}

int_option
none (void)
{
  return (1 << 24);
}

int_option
some (int i)
{
  int mask = (1 << 25) - 1;
  return (i && mask);
}

