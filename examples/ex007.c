#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

enum t {
  CHAUD,
  FROID
};

struct oscillateur_mem {

  int aux__2_next6;
  enum t aux__8_next5;
  int aux__10_next4;
  int aux__12_next3;
  int aux__14_next2;
  int aux__16_next1;
};

void oscillateur_init (struct oscillateur_mem* mem) {
  mem->aux__2_next6 = 1;
  
  mem->aux__8_next5 = CHAUD;
  
  mem->aux__10_next4 = 1;
  
  mem->aux__12_next3 = 0;
  
  mem->aux__14_next2 = 1;
  
  mem->aux__16_next1 = 0;
}

int oscillateur (struct oscillateur_mem* mem, int lo, int hi) {
  int aux__16;
  int aux__15;
  int aux__14;
  int aux__13;
  int aux__12;
  int aux__11;
  int aux__10;
  int aux__9;
  enum t aux__8;
  enum t aux__7;
  int aux__6;
  int aux__5;
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  enum t state;
  int x;
  enum t switch_1;
  enum t switch_2;
  enum inductive_bool switch_3;
  enum inductive_bool switch_4;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next6;
  
  aux__8 = mem->aux__8_next5;
  
  aux__9 = 0;
  
  aux__10 = mem->aux__10_next4;
  
  aux__12 = mem->aux__12_next3;
  
  aux__13 = 0;
  
  aux__14 = mem->aux__14_next2;
  
  aux__16 = mem->aux__16_next1;
  
  state = aux__2 ? CHAUD : aux__8;
  
  switch (state) {
    case CHAUD: {
      switch_1 = (aux__10 ? 0 : aux__12 + 1);
      break;
    }
    case FROID: {
      switch_1 = (aux__14 ? 0 : aux__16 - 1);
      break;
    }
  };
  
  x = switch_1;
  
  aux__3 = (x >= hi);
  
  aux__4 = (x >= hi);
  
  aux__5 = (x <= lo);
  
  aux__6 = (x <= lo);
  
  switch ((x >= hi)) {
    case TRUE: {
      switch_3 = FROID;
      break;
    }
    case FALSE: {
      switch_3 = CHAUD;
      break;
    }
  };
  
  switch ((x <= lo)) {
    case TRUE: {
      switch_4 = CHAUD;
      break;
    }
    case FALSE: {
      switch_4 = FROID;
      break;
    }
  };
  
  switch (state) {
    case CHAUD: {
      switch_2 = switch_3;
      break;
    }
    case FROID: {
      switch_2 = switch_4;
      break;
    }
  };
  
  aux__7 = switch_2;
  
  aux__11 = x;
  
  aux__15 = x;
  
  mem->aux__2_next6 = aux__1;
  
  mem->aux__8_next5 = aux__7;
  
  mem->aux__10_next4 = aux__9;
  
  mem->aux__12_next3 = aux__11;
  
  mem->aux__14_next2 = aux__13;
  
  mem->aux__16_next1 = aux__15;
  
  return x;
}

struct check_mem {
  struct oscillateur_mem oscillateur_next1;
};

void check_init (struct check_mem* mem) {
  oscillateur_init(&(mem->oscillateur_next1));
}

int check (struct check_mem* mem) {
  int aux__17;
  int o;
  int call_1;
  
  call_1 = oscillateur(&(mem->oscillateur_next1), -(5), 5);
  
  aux__17 = call_1;
  
  o = aux__17;
  
  return o;
}

int main (int argc, char* argv[]) {
  struct check_mem mem;
  int res;
  
  check_init(&(mem));
  
  if ((argc < 1)) {
    printf("Error : %d needed arguments were not provided", 0);
    
    exit(1);
  };
  
  while (1) {
    res = check(&(mem));
    
    printf("%d", res);
    
    fflush(0);
  };
}
