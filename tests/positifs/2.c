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

struct counter_mem {
  int aux__2_next4;
  int aux__4_next3;
  int aux__6_next2;
  int aux__8_next1;
};

void counter_init (struct counter_mem* mem) {
  mem->aux__2_next4 = 1;
  
  mem->aux__4_next3 = 0;
  
  mem->aux__6_next2 = 1;
  
  mem->aux__8_next1 = 0;
}

int counter (struct counter_mem* mem, int res, int tick) {
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int o;
  enum inductive_bool switch_1;
  enum inductive_bool switch_2;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next4;
  
  aux__4 = mem->aux__4_next3;
  
  aux__5 = 0;
  
  aux__6 = mem->aux__6_next2;
  
  aux__8 = mem->aux__8_next1;
  
  switch (tick) {
    case TRUE: {
      switch_2 = aux__2 ? 1 : (aux__4 + 1);
      break;
    }
    case FALSE: {
      switch_2 = aux__6 ? 0 : aux__8;
      break;
    }
  };
  
  switch (res) {
    case TRUE: {
      switch_1 = 0;
      break;
    }
    case FALSE: {
      switch_1 = switch_2;
      break;
    }
  };
  
  o = switch_1;
  
  aux__3 = o;
  
  aux__7 = o;
  
  mem->aux__2_next4 = aux__1;
  
  mem->aux__4_next3 = aux__3;
  
  mem->aux__6_next2 = aux__5;
  
  mem->aux__8_next1 = aux__7;
  
  return o;
}

struct main1_mem {

  struct counter_mem counter_next1;
  int aux__10_next8;
  int aux__12_next7;
  int aux__15_next6;
  int aux__17_next5;
};

void main1_init (struct main1_mem* mem) {
  mem->aux__10_next8 = 1;
  
  mem->aux__12_next7 = 0;
  
  mem->aux__15_next6 = 1;
  
  mem->aux__17_next5 = 0;
  
  counter_init(&(mem->counter_next1));
}

int main1 (struct main1_mem* mem) {
  int aux__17;
  int aux__16;
  int aux__15;
  int aux__14;
  int aux__13;
  int aux__12;
  int aux__11;
  int aux__10;
  int aux__9;
  int half;
  int y;
  int call_1;
  
  aux__9 = 0;
  
  aux__10 = mem->aux__10_next8;
  
  aux__12 = mem->aux__12_next7;
  
  aux__14 = 0;
  
  aux__15 = mem->aux__15_next6;
  
  aux__17 = mem->aux__17_next5;
  
  half = aux__15 ? 1 : aux__17;
  
  call_1 = counter(&(mem->counter_next1), aux__10 ? 0 : aux__12, half);
  
  aux__13 = call_1;
  
  aux__16 = !(half);
  
  y = aux__13;
  
  aux__11 = (y > 10);
  
  mem->aux__10_next8 = aux__9;
  
  mem->aux__12_next7 = aux__11;
  
  mem->aux__15_next6 = aux__14;
  
  mem->aux__17_next5 = aux__16;
  
  return y;
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
