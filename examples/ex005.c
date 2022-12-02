#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

enum typ__1 {
  CHAUD,
  FROID
};

struct oscillateur_mem {

  int aux__2_next6;
  enum typ__1 aux__4_next5;
  int aux__6_next4;
  int aux__8_next3;
  int aux__10_next2;
  int aux__12_next1;
};

void oscillateur_init (struct oscillateur_mem* mem) {
  mem->aux__2_next6 = 1;
  
  mem->aux__4_next5 = CHAUD;
  
  mem->aux__6_next4 = 1;
  
  mem->aux__8_next3 = 0;
  
  mem->aux__10_next2 = 1;
  
  mem->aux__12_next1 = 0;
}

int oscillateur (struct oscillateur_mem* mem, int lo, int hi) {
  int aux__12;
  int aux__11;
  int aux__10;
  int aux__9;
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  enum typ__1 aux__4;
  enum typ__1 aux__3;
  int aux__2;
  int aux__1;
  enum typ__1 state__2;
  int cond__4;
  int cond__3;
  int x;
  enum typ__1 switch_1;
  enum typ__1 switch_2;
  enum inductive_bool switch_3;
  enum inductive_bool switch_4;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next6;
  
  aux__4 = mem->aux__4_next5;
  
  aux__5 = 0;
  
  aux__6 = mem->aux__6_next4;
  
  aux__8 = mem->aux__8_next3;
  
  aux__9 = 0;
  
  aux__10 = mem->aux__10_next2;
  
  aux__12 = mem->aux__12_next1;
  
  state__2 = aux__2 ? CHAUD : aux__4;
  
  switch (state__2) {
    case CHAUD: {
      switch_1 = aux__6 ? 0 : (aux__8 + 1);
      break;
    }
    case FROID: {
      switch_1 = aux__10 ? 0 : (aux__12 - 1);
      break;
    }
  };
  
  x = switch_1;
  
  cond__4 = (x <= lo);
  
  cond__3 = (x >= hi);
  
  aux__7 = x;
  
  aux__11 = x;
  
  switch (cond__3) {
    case TRUE: {
      switch_3 = FROID;
      break;
    }
    case FALSE: {
      switch_3 = CHAUD;
      break;
    }
  };
  
  switch (cond__4) {
    case TRUE: {
      switch_4 = CHAUD;
      break;
    }
    case FALSE: {
      switch_4 = FROID;
      break;
    }
  };
  
  switch (state__2) {
    case CHAUD: {
      switch_2 = switch_3;
      break;
    }
    case FROID: {
      switch_2 = switch_4;
      break;
    }
  };
  
  aux__3 = switch_2;
  
  mem->aux__2_next6 = aux__1;
  
  mem->aux__4_next5 = aux__3;
  
  mem->aux__6_next4 = aux__5;
  
  mem->aux__8_next3 = aux__7;
  
  mem->aux__10_next2 = aux__9;
  
  mem->aux__12_next1 = aux__11;
  
  return x;
}

struct check_mem {
  struct oscillateur_mem oscillateur_next1;
};

void check_init (struct check_mem* mem) {
  oscillateur_init(&(mem->oscillateur_next1));
}

int check (struct check_mem* mem) {
  int aux__13;
  int o;
  int call_1;
  
  call_1 = oscillateur(&(mem->oscillateur_next1), -(5), 5);
  
  aux__13 = call_1;
  
  o = aux__13;
  
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
