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

int incr (struct incr_mem* mem, int tic) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int cpt;
  enum inductive_bool switch_1;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  switch (tic) {
    case TRUE: {
      switch_1 = 1;
      break;
    }
    case FALSE: {
      switch_1 = 0;
      break;
    }
  };
  
  cpt = (aux__2 ? 0 : aux__4 + switch_1);
  
  aux__3 = cpt;
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return cpt;
}

struct check_mem {
  struct incr_mem incr_next1;
  int aux__7_next4;
  int aux__9_next3;
};

void check_init (struct check_mem* mem) {
  mem->aux__7_next4 = 1;
  
  mem->aux__9_next3 = 0;
  
  incr_init(&(mem->incr_next1));
}

int check (struct check_mem* mem, int x) {
  int aux__9;
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  int cpt;
  int ok;
  int call_1;
  
  switch (x) {
    case 1: {
      incr_init(&(mem->incr_next1));
      break;
    }
  };
  
  call_1 = incr(&(mem->incr_next1), x);
  
  aux__5 = call_1;
  
  aux__6 = 0;
  
  aux__7 = mem->aux__7_next4;
  
  aux__9 = mem->aux__9_next3;
  
  cpt = aux__5;
  
  ok = (aux__7 ? 0 : aux__9 <= cpt);
  
  aux__8 = cpt;
  
  mem->aux__7_next4 = aux__6;
  
  mem->aux__9_next3 = aux__8;
  
  return ok;
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
    
    sleep(1);
  };
}
