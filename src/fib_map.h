#ifndef DADS3_FIB_MAP
#define DADS3_FIB_MAP

struct node;
typedef struct node node;

node* get_node(void *);
void save_to_map(void *);
void delete_from_map(void *);

#endif
