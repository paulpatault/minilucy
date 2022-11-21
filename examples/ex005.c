enum inductive_bool{TRUE, FALSE};
enum typ__1{AWAIT, RUN};
struct aux_mem {

  int aux__2_next6;
  enum typ__1 aux__4_next5;
  int aux__6_next4;
  int aux__8_next3;
  int aux__10_next2;
  int aux__12_next1;
};

void aux_init (struct aux_mem* mem) {
  mem->aux__2_next6 = 1;
  
  mem->aux__4_next5 = AWAIT;
  
  mem->aux__6_next4 = 1;
  
  mem->aux__8_next3 = 0;
  
  mem->aux__10_next2 = 1;
  
  mem->aux__12_next1 = 0;
}

int aux (struct aux_mem* mem, int lo, int hi) {
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
  int switch_3;
  int switch_4;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next6;
  
  aux__4 = mem->aux__4_next5;
  
  aux__5 = 0;
  
  aux__6 = mem->aux__6_next4;
  
  aux__8 = mem->aux__8_next3;
  
  aux__9 = 0;
  
  aux__10 = mem->aux__10_next2;
  
  aux__12 = mem->aux__12_next1;
  
  state__2 = aux__2 ? AWAIT : aux__4;
  
  switch (state__2) {
    case AWAIT: {
      switch_1 = aux__6 ? 0 : (aux__8 + 1);
      break;
    }
    case RUN: {
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
    case 1: {
      switch_3 = RUN;
      break;
    }
    case 0: {
      switch_3 = AWAIT;
      break;
    }
  };
  
  switch (cond__4) {
    case 1: {
      switch_4 = AWAIT;
      break;
    }
    case 0: {
      switch_4 = RUN;
      break;
    }
  };
  
  switch (state__2) {
    case AWAIT: {
      switch_2 = switch_3;
      break;
    }
    case RUN: {
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

struct main1_mem {
  struct aux_mem aux_next1;
};

void main1_init (struct main1_mem* mem) {
  aux_init(&(mem->aux_next1));
}

int main1 (struct main1_mem* mem) {
  int aux__13;
  int x;
  int o;
  int call_1;
  
  o = 1;
  
  call_1 = aux(&(mem->aux_next1), -(5), 5);
  
  aux__13 = call_1;
  
  x = aux__13;
  
  return o;
}

int main (int argc, char* argv[]) {
  struct main1_mem mem;
  
  main1_init(&(mem));
  
  while (1) {
    main1(&(mem));
  };
}
