struct counting_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void counting_init (struct counting_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

struct counting_ret {
  int o;
  int y;
};

struct counting_ret counting (struct counting_mem* mem, int tick, int top) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int v;
  struct counting_ret ret_;
  int switch_1;
  int switch_2;
  
  switch (top) {
    case 1: {
      switch_1 = 1;
      break;
    }
    case 0: {
      switch_1 = 0;
      break;
    }
  };
  
  v = switch_1;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  switch (tick) {
    case 1: {
      switch_2 = v;
      break;
    }
    case 0: {
      switch_2 = aux__2 ? 0 : (aux__4 + v);
      break;
    }
  };
  
  ret_.o = switch_2;
  
  ret_.y = v;
  
  aux__3 = ret_.o;
  
  mem->aux__2_next2 = 0;
  
  mem->aux__4_next1 = ret_.o;
  
  return ret_;
}

struct check_mem {
  struct counting_mem counting_next1;
};

void check_init (struct check_mem* mem) {
  counting_init(&(mem->counting_next1));
}

struct tuple_ty_1 {
  int tuple_field_1;
  int tuple_field_2;
};

int check (struct check_mem* mem, int x) {
  int aux__5;
  int aux__6;
  int y;
  int z;
  int OK;
  int switch_3;
  struct counting_ret call_1;
  struct tuple_ty_1 tuple_ty_1__;
  
  switch (x) {
    case 1: {
      switch_3 = 0;
      break;
    }
    case 0: {
      switch_3 = 1;
      break;
    }
  };
  
  OK = switch_3;
  
  call_1 = counting(&(mem->counting_next1), x, x);
  
  aux__5 = call_1.o;
  
  aux__6 = call_1.y;
  
  tuple_ty_1__.tuple_field_1 = aux__5;
  
  tuple_ty_1__.tuple_field_2 = aux__6;
  
  y = tuple_ty_1__.tuple_field_1;
  
  z = tuple_ty_1__.tuple_field_2;
  
  return OK;
}

int main (int argc, char* argv[]) {
  struct check_mem mem;
  
  check_init(&(mem));
  
  while (1) {
    check(&(mem));
  };
}
