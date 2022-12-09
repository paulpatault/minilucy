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

struct f_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void f_init (struct f_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

int f (struct f_mem* mem, int i, int c) {
  int aux__5;
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int t;
  int x;
  int call_1;
  
  x = i;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  t = aux__2 ? 0 : aux__4;
  
  aux__3 = t;
  
  printf("%d", t);
  
  aux__5 = call_1;
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return x;
}

int main (int argc, char* argv[]) {
  struct f_mem mem;
  int argv_0;
  int argv_1;
  int res;
  
  f_init(&(mem));
  
  while (1) {
    argv_0 = int_read();
    
    argv_1 = int_read();
    
    res = f(&(mem), argv_0, argv_1);
    
    printf("%d", res);
    
    fflush(0);
    
    usleep(333333);
  };
}
