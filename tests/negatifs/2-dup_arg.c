#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

int int_read() {
  int var;
  scanf("%d", &var);
  return var;
}

enum inductive_bool {
  FALSE,
  TRUE
};

int main0 (int x) {
  int y;
  
  y = x;
  
  return y;
}

int main (int argc, char* argv[]) {
  int argv_0;
  int res;
  
  while (1) {
    argv_0 = int_read();
    
    res = main0(argv_0);
    
    printf("%d ", res);
    
    fflush(0);
    
    usleep(333333);
  };
}
