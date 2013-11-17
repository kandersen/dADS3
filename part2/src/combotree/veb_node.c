#include "veb_node.h"


typedef struct vEB_node vEB_node;
typedef struct vEB_tree vEB_tree;

void
sort (uint24_option * lo, uint24_option * hi)
{
     if (*lo > *hi) 
     {
          uint24_option temp = *lo;
          *lo = *hi;
          *hi = temp;
     }          
}

bool
fits (uint8_t const bits, uint24_option const value)
{
     return !is_none(value) && (value >> bits) == 0;
}

uint24_option
hi_value (uint8_t const universe_bits, uint24_option const value)
{
     if (is_none(value))
          return none();

     uint8_t lo_bits = universe_bits >> 1;
     uint8_t hi_bits = lo_bits + (universe_bits & 1);
     uint32_t bit_mask = (1 << hi_bits) - 1;

     return some((value >> lo_bits) & bit_mask);
}

uint24_option
lo_value (uint8_t const universe_bits, uint24_option const value)
{
     if (is_none(value))
          return none();

     uint8_t lo_bits = universe_bits >> 1;
     uint32_t bit_mask = (1 << lo_bits) - 1;

     return some(bit_mask & value);
}

uint24_option
combine_value (uint8_t const universe_bits, uint24_option const hi, uint24_option const lo)
{
     if (is_none(hi) || is_none(lo))
          return none();

     uint8_t lo_bits = universe_bits >> 1;
     uint8_t hi_bits = lo_bits + (universe_bits & 1);
     uint32_t hi_mask = (1 << hi_bits) - 1;
     uint32_t lo_mask = (1 << lo_bits) - 1;

     return ((hi & hi_mask) << lo_bits) + (lo & lo_mask);
}

vEB_node * 
vEB_node_init (uint8_t const universe_bits)
{
     uint8_t lo_bits = universe_bits >> 1;
     uint8_t hi_bits = lo_bits + (universe_bits & 1);
     uint32_t recursive_trees = (universe_bits > 1) ? 1 << hi_bits : 0;
    
     vEB_node const_initialization = {
          .universe_bits = universe_bits,
          .min = none(),
          .max = none(),
          .top = (universe_bits > 1) ? vEB_init(hi_bits) : NULL,
          .bottom = (universe_bits > 1) ? malloc(sizeof(vEB_node)*recursive_trees) : NULL
     };
     
     for (int i = 0; i < recursive_trees; i++)
          const_initialization.bottom[i] = vEB_init(lo_bits);

     vEB_node * node = (vEB_node *) malloc(sizeof(vEB_node));
     memcpy(node, &const_initialization, sizeof(vEB_node));
     
     return node;
}

bool 
vEB_node_insert (vEB_tree *const node, uint24_option const value)
{
     /* no value to insert or value is kept directly in the node */
     if (is_none(value) ||!fits(node->universe_bits, value) || value == node->min || value == node->max)
          return false;
     
     /* keep min & max outside recursive, min == max indicate size == 0 or 1 ~ need to keep the value in node */
     if (node->min == node->max || node->universe_bits == 1) 
     {
          bool insert_successfull = false;
          if (value < node->min || is_none(node->min)) 
               node->min = value, insert_successfull = true;
          if (value > node->max || is_none(node->max))
               node->max = value, insert_successfull = true;
          return insert_successfull;
     }

     uint24_option a, b, to_insert = value;     

     sort(&(node->min), &to_insert);
     sort(&to_insert, &(node->max));

     a = hi_value(node->universe_bits, to_insert);
     b = lo_value(node->universe_bits, to_insert);

     if (vEB_minimum(node->bottom[a]) == none())
          vEB_insert(node->top, a);

     return vEB_insert(node->bottom[a], b);
}

bool 
vEB_node_delete (vEB_tree *const node, uint24_option const value)
{
     /* empty node or 'empty' value */
     if (is_none(value) || !fits(node->universe_bits, value) || is_none(node->min))
          return false;

     /* leaf vEB node */
     if ( node->universe_bits == 1 || is_none(vEB_minimum(node->top)))
     {
          bool success = false;
          if (value == node->min) node->min = none(), success = true;
          if (value == node->max) node->max = none(), success = true;
          if (node->min == none()) node->min = node->max;
          if (node->max == none()) node->max = node->min;
          return success;
     }
          
     /* value falls outside contained range */
     if ( value < node->min || value > node->max)
          return false;

     uint24_option a = none(), b = none();

     /* value falls inside recursively contained range - NOT MIN or MAX*/
     if ( node->min < value && value < node->max)
     {
          a = hi_value(node->universe_bits, value);
          b = lo_value(node->universe_bits, value);    
     } 
     /* value is min - extract new min from recursive structure */
     else if (value == node->min)
     {
          a = vEB_minimum(node->top);
          b = vEB_minimum(node->bottom[a]);
          node->min = combine_value(node->universe_bits, a, b);     
     }
     /* value is max - similar to prev case */
     else if (value == node->max)
     {
          a = vEB_maximum(node->top);
          b = vEB_maximum(node->bottom[a]);
          node->max = combine_value(node->universe_bits, a, b);
     }
     
     bool success =  vEB_delete(node->bottom[a], b);     

     if (vEB_minimum(node->bottom[a]) == none())
          vEB_delete(node->top, a);
     
     return success;
}

bool 
vEB_node_contains (vEB_tree const *const node, uint24_option const value)
{
     if (is_none(value) || !fits(node->universe_bits, value))
          return false;

     bool contains = false;

     contains |= (value == node->min);
     contains |= (value == node->max);

     if (node->universe_bits > 1)
     {
          uint24_option a = hi_value(node->universe_bits, value);
          uint24_option b = lo_value(node->universe_bits, value);    
          contains |= vEB_contains(node->bottom[a], b);
     }

     return contains;
}

uint24_option 
vEB_node_minimum (vEB_tree const *const node)
{
     return some(node->min);
}

uint24_option 
vEB_node_maximum (vEB_tree const *const node)
{
     return some(node->max);
}

uint24_option 
vEB_node_pred (vEB_tree const *const node, uint24_option const value)
{
     if (is_none(value) || !fits(node->universe_bits, value))
         return none();
     
     uint24_option a = hi_value(node->universe_bits, value);
     uint24_option b = lo_value(node->universe_bits, value);    
     uint24_option pred_a = node->universe_bits > 1 ? vEB_minimum(node->top) : none();
     uint24_option pred_b = node->universe_bits > 1 ? vEB_minimum(node->bottom[a]) : none();

     /* a'th subtree has a pred */
     if (is_some(pred_b) && pred_b < b && node->universe_bits > 1) 
     {
          pred_a = a;
          pred_b = vEB_pred(node->bottom[a], b);
     }
     /* some subtree has a pred */
     else if (is_some(pred_a) && pred_a < a && node->universe_bits > 1)
     {
          pred_a = vEB_pred(node->top, a);
          pred_b = vEB_maximum(node->bottom[pred_a]);
     } 
     /* no luck in recursive structure */
     else
     {
          pred_a = pred_b = none();
     }
     
     uint24_option pred = combine_value(node->universe_bits, pred_a, pred_b);

     /* consider min and max for pred replacements */
     if (node->min < value && (is_none(pred) || node->min > pred) && is_some(node->min))
          pred = node->max;

     if (node->max < value && (is_none(pred) || node->max > pred) && is_some(node->max))
          pred = node->max;

     return pred;
}

uint24_option 
vEB_node_succ (vEB_tree const *const node, uint24_option const value)
{
     if (is_none(value) || !fits(node->universe_bits, value))
         return none();

     uint24_option a = hi_value(node->universe_bits, value);
     uint24_option b = lo_value(node->universe_bits, value);    
     uint24_option succ_a = node->universe_bits > 1 ? vEB_maximum(node->top) : none();
     uint24_option succ_b = node->universe_bits > 1 ? vEB_maximum(node->bottom[a]) : none();

     /* a'th subtree has a succ */
     if (is_some(succ_b) && succ_b > b && node->universe_bits > 1) 
     {
          succ_a = a;
          succ_b = vEB_succ(node->bottom[a], b);
     }
     /* some subtree has a succ */
     else if (is_some(succ_a) && succ_a > a && node->universe_bits > 1)
     {
          succ_a = vEB_succ(node->top, a);
          succ_b = vEB_minimum(node->bottom[succ_a]);
     } 
     /* no luck in recursive structure */
     else
     {
          succ_a = succ_b = none();
     }
     
     uint24_option succ = combine_value(node->universe_bits, succ_a, succ_b);

     /* consider min and max for succ replacements */
     if (node->min > value && (is_none(succ) || node->min < succ) && is_some(node->min))
          succ = node->max;

     if (node->max > value && (is_none(succ) || node->max < succ) && is_some(node->max))
          succ = node->max;

     return succ;
}
