#include <stdlib.h>
#include "fib_map.h"
#include <unordered_map>

typedef std::unordered_map<void*, node*> MyMap;

MyMap c1;

node* get_node(void* i) {
  std::unordered_map<void*,node*>::const_iterator got = c1.find (i);
  if ( got == c1.end() )
    return NULL;
  else
    return got->second;
}

void save_to_map(void* i, node* n) {
  c1.insert(MyMap::value_type(i, n));
}

void delete_from_map(void* i) {
  c1.erase(i);
}

