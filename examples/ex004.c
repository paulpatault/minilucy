#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

struct counting_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void counting_init (struct counting_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

int counting (struct counting_mem* mem, int tick, int top) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int v;
  int o;
  enum inductive_bool switch_1;
  enum inductive_bool switch_2;
  
  switch (top) {
    case TRUE: {
      switch_1 = 1;
      break;
    }
    case FALSE: {
      switch_1 = 0;
      break;
    }
  };
  
  v = switch_1;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  switch (tick) {
    case TRUE: {
      switch_2 = v;
      break;
    }
    case FALSE: {
      switch_2 = aux__2 ? 0 : (aux__4 + v);
      break;
    }
  };
  
  o = switch_2;
  
  aux__3 = o;
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return o;
}

struct check_mem {
  struct counting_mem counting_next1;
};

void check_init (struct check_mem* mem) {
  counting_init(&(mem->counting_next1));
}

int check (struct check_mem* mem, int x) {
  int aux__5;
  int y;
  int call_1;
  
  call_1 = counting(&(mem->counting_next1), x, x);
  
  aux__5 = call_1;
  
  y = (aux__5 < 2);
  
  return y;
}

int main (int argc, char* argv[]) {
  struct check_mem mem;
  int argv_0;
  int res;
  
  check_init(&(mem));
  
  if ((argc < 2)) {
    printf("Error : %d needed arguments were not provided", 1);
    
    exit(1);
  };
  
  argv_0 = atoi(argv[1]);
  
  while (1) {
    res = check(&(mem), argv_0);
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
