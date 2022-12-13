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

int check (int x, int y) {
  int aux__2;
  int aux__1;
  int o;
  int call_1;
  int call_2;
  
  o = (x == y);
  
  printf("%d", x);
  
  aux__1 = call_1;
  
  printf("\"c%doucou\"", 1);
  
  aux__2 = call_2;
  
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
    
    usleep(333333);
  };
}
