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

enum typ__1 {
  ONE,
  TWO
};

struct count_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void count_init (struct count_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

int count (struct count_mem* mem) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int o;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  o = aux__2 ? 0 : aux__4;
  
  aux__3 = (o + 1);
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return o;
}

int f () {
  int x;
  
  x = 1;
  
  return x;
}

int g () {
  int x;
  
  x = 0;
  
  return x;
}

struct main1_mem {

  struct count_mem count_next4;
  struct count_mem count_next3;
  int aux__8_next4;
  enum typ__1 aux__10_next3;
};

void main1_init (struct main1_mem* mem) {
  mem->aux__8_next4 = 1;
  
  mem->aux__10_next3 = ONE;
  
  count_init(&(mem->count_next4));
  
  count_init(&(mem->count_next3));
}

int main1 (struct main1_mem* mem) {
  int aux__12;
  int aux__11;
  enum typ__1 aux__10;
  enum typ__1 aux__9;
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  enum typ__1 state__2;
  int cond__4;
  int cond__3;
  int o;
  int call_1;
  int call_2;
  int call_3;
  int call_4;
  enum typ__1 switch_1;
  enum typ__1 switch_2;
  enum inductive_bool switch_3;
  enum inductive_bool switch_4;
  
  call_1 = count(&(mem->count_next4));
  
  aux__5 = call_1;
  
  call_2 = count(&(mem->count_next3));
  
  aux__6 = call_2;
  
  aux__7 = 0;
  
  aux__8 = mem->aux__8_next4;
  
  aux__10 = mem->aux__10_next3;
  
  call_3 = f();
  
  aux__11 = call_3;
  
  call_4 = g();
  
  aux__12 = call_4;
  
  state__2 = aux__8 ? ONE : aux__10;
  
  cond__4 = ((aux__5 % 5) == 0);
  
  cond__3 = ((aux__6 % 20) == 0);
  
  switch (state__2) {
    case ONE: {
      switch_1 = aux__11;
      break;
    }
    case TWO: {
      switch_1 = aux__12;
      break;
    }
  };
  
  o = switch_1;
  
  switch (cond__3) {
    case TRUE: {
      switch_3 = TWO;
      break;
    }
    case FALSE: {
      switch_3 = ONE;
      break;
    }
  };
  
  switch (cond__4) {
    case TRUE: {
      switch_4 = ONE;
      break;
    }
    case FALSE: {
      switch_4 = TWO;
      break;
    }
  };
  
  switch (state__2) {
    case ONE: {
      switch_2 = switch_3;
      break;
    }
    case TWO: {
      switch_2 = switch_4;
      break;
    }
  };
  
  aux__9 = switch_2;
  
  mem->aux__8_next4 = aux__7;
  
  mem->aux__10_next3 = aux__9;
  
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
