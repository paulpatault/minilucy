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
  int aux__2_next4;
  int aux__4_next3;
  int aux__6_next2;
  int aux__8_next1;
};

void f_init (struct f_mem* mem) {
  mem->aux__2_next4 = 1;
  
  mem->aux__4_next3 = 1;
  
  mem->aux__6_next2 = 0;
  
  mem->aux__8_next1 = 0;
}

int f (struct f_mem* mem) {
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int x;
  int o;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next4;
  
  aux__3 = 0;
  
  aux__4 = mem->aux__4_next3;
  
  aux__6 = mem->aux__6_next2;
  
  aux__8 = mem->aux__8_next1;
  
  x = aux__2 ? 1 : aux__8;
  
  aux__7 = aux__4 ? 0 : aux__6;
  
  o = x;
  
  aux__5 = x;
  
  mem->aux__2_next4 = aux__1;
  
  mem->aux__4_next3 = aux__3;
  
  mem->aux__6_next2 = aux__5;
  
  mem->aux__8_next1 = aux__7;
  
  return o;
}

struct main1_mem {
  struct f_mem f_next1;
};

void main1_init (struct main1_mem* mem) {
  f_init(&(mem->f_next1));
}

int main1 (struct main1_mem* mem) {
  int aux__9;
  int o;
  int call_1;
  
  call_1 = f(&(mem->f_next1));
  
  aux__9 = call_1;
  
  o = aux__9;
  
  return o;
}

int main (int argc, char* argv[]) {
  struct main1_mem mem;
  int res;
  
  main1_init(&(mem));
  
  while (1) {
    res = main1(&(mem));
    
    printf("%d", res);
    
    fflush(0);
    
    usleep(333333);
  };
}
