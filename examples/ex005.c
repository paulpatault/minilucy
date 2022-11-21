struct aux_mem {
  int aux__2_next4;
  int aux__4_next3;
  int aux__6_next2;
  int aux__8_next1;
};

void aux_init (struct aux_mem* mem) {
  mem->aux__2_next4 = 1;
  
  mem->aux__4_next3 = 0;
  
  mem->aux__6_next2 = 1;
  
  mem->aux__8_next1 = 0;
}

enum typ__1{AWAIT, RUN};
enum inductive_bool{TRUE, FALSE};
int aux (struct aux_mem* mem, int lo, int hi) {
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int cond__4;
  int cond__3;
  enum typ__1 state__2;
  int x;
  enum typ__1 switch_1;
  int switch_2;
  int switch_3;
  enum typ__1 switch_4;
  
  switch (cond__3) {
    case 1: {
      switch_2 = RUN;
      break;
    }
    case 0: {
      switch_2 = AWAIT;
      break;
    }
  };
  
  switch (cond__4) {
    case 1: {
      switch_3 = AWAIT;
      break;
    }
    case 0: {
      switch_3 = RUN;
      break;
    }
  };
  
  switch (state__2) {
    case AWAIT: {
      switch_1 = switch_2;
      break;
    }
    case RUN: {
      switch_1 = switch_3;
      break;
    }
  };
  
  state__2 = switch_1;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next4;
  
  aux__4 = mem->aux__4_next3;
  
  aux__5 = 0;
  
  aux__6 = mem->aux__6_next2;
  
  aux__8 = mem->aux__8_next1;
  
  switch (state__2) {
    case AWAIT: {
      switch_4 = aux__2 ? 0 : (aux__4 + 1);
      break;
    }
    case RUN: {
      switch_4 = aux__6 ? 0 : (aux__8 - 1);
      break;
    }
  };
  
  x = switch_4;
  
  cond__4 = (x >= hi);
  
  cond__3 = (x <= lo);
  
  aux__3 = x;
  
  aux__7 = x;
  
  mem->aux__2_next4 = 0;
  
  mem->aux__4_next3 = x;
  
  mem->aux__6_next2 = 0;
  
  mem->aux__8_next1 = x;
  
  return x;
}

struct main1_mem {
  struct aux_mem aux_next2;
  struct aux_mem aux_next1;
};

void main1_init (struct main1_mem* mem) {
  aux_init(&(mem->aux_next2));
  
  aux_init(&(mem->aux_next1));
}

enum typ__1{AWAIT, RUN};
enum inductive_bool{TRUE, FALSE};
int main1 (struct main1_mem* mem) {
  int aux__10;
  int aux__9;
  int x;
  int y;
  int o;
  int call_1;
  int call_2;
  
  call_1 = aux(&(mem->aux_next2), 1, 3);
  
  aux__9 = call_1;
  
  call_2 = aux(&(mem->aux_next1), 1, 3);
  
  aux__10 = call_2;
  
  x = aux__9;
  
  y = aux__10;
  
  o = (x == y);
  
  return o;
}

int main (int argc, char* argv[]) {
  struct main1_mem mem;
  
  main1_init(&(mem));
  
  while (1) {
    main1(&(mem));
  };
}
