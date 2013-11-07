#include <stdlib.h>
#include <iostream>
static int maxKey = 1000;
typedef int Key;

class Item { 
private:
  Key keyval; 
  float info;
public:
  Item() {
    keyval = maxKey;
  } 

  Key key() {
    return keyval;
  }

  int null() {
    return keyval == maxKey;
  }

  void rand() {
    keyval = 1000*::rand()/RAND_MAX;   
    info = 1.0*::rand()/RAND_MAX;
  }  
  
  int scan(std::istream& is = std::cin) {
    return (is >> keyval >> info) != 0;
  }
  
  void show(std::ostream& os = std::cout) {
    os << keyval << " " << info << std::endl;
  }
};
