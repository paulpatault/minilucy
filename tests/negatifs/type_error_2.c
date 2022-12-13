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

enum t {
  A,
  B
};

void main_init (struct main_mem* mem) {
  ;
}

int main (enum t x) {
  int o;
  enum t switch_1;
  
  switch (x) {
    case A: {
      switch_1 = 1;
      break;
    }
    case B: {
      switch_1 = 0;
      break;
    }
  };
  
  o = switch_1;
  
  return o;
}

int main (int argc, char* argv[]) {
  int argv_0;
  int res;
  
  while (1) {
    argv_0 = int_read();
    
    res = main(argv_0);
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
