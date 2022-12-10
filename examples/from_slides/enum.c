#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

enum mode {
  RISING,
  FALLING,
  STABLE
};

struct counter_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void counter_init (struct counter_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

int counter (struct counter_mem* mem, enum mode v) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int py;
  int y;
  enum mode switch_1;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  py = aux__2 ? 0 : aux__4;
  
  printf ("%d\n", v);
  switch (v) {
    case RISING: {
      switch_1 = (py + 1);
      break;
    }
    case FALLING: {
      switch_1 = (py - 1);
      break;
    }
    case STABLE: {
      switch_1 = py;
      break;
    }
  };
  
  y = switch_1;
  printf ("%d\n", y);
  
  aux__3 = y;
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return y;
}


int main (int argc, char* argv[]) {
  struct counter_mem mem;
  int argv_0;
  int res;
  
  counter_init(&(mem));
  
  if ((argc < 2)) {
    printf("Error : %d needed arguments were not provided", 1);
    
    exit(1);
  };
  
  argv_0 = atoi(argv[1]);
  
  while (1) {
    argv_0 = mread();
    res = counter(&(mem), argv_0);
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
