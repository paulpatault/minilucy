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

struct incr_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void incr_init (struct incr_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

int incr (struct incr_mem* mem) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int cpt;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  cpt = (aux__2 ? 0 : aux__4 + 1);
  
  aux__3 = cpt;
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return cpt;
}

struct check_mem {
  struct incr_mem incr_next1;
};

void check_init (struct check_mem* mem) {
  incr_init(&(mem->incr_next1));
}

int check (struct check_mem* mem, int x) {
  int aux__5;
  int o;
  int call_1;
  
  if (x) {
    incr_init(&(mem->incr_next1));
  };
  
  call_1 = incr(&(mem->incr_next1));
  
  aux__5 = call_1;
  
  o = aux__5;
  
  return o;
}

int main (int argc, char* argv[]) {
  struct check_mem mem;
  int argv_0;
  int res;
  
  check_init(&(mem));
  
  while (1) {
    argv_0 = int_read();
    
    res = check(&(mem), argv_0);
    
    printf("%d", res);
    
    fflush(0);
    
    usleep(333333);
  };
}
