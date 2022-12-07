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

void check_init (struct check_mem* mem) {
  ;
}

int check (int x, int y) {
  int aux__1;
  int o;
  int call_1;
  
  o = (x == y);
  
  printf("%d", x);
  
  aux__1 = call_1;
  
  return o;
}

int main (int argc, char* argv[]) {
  int argv_0;
  int argv_1;
  int res;
  
  while (1) {
    argv_0 = int_read();
    
    argv_1 = int_read();
    
    res = check(argv_0, argv_1);
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
