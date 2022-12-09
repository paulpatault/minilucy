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

int m0 () {
  int x;
  int o;
  enum inductive_bool switch_1;
  enum inductive_bool switch_2;
  
  x = 1;
  
  switch (x) {
    case TRUE: {
      switch_2 = 1;
      break;
    }
    case FALSE: {
      switch_2 = 2;
      break;
    }
  };
  
  switch (x) {
    case TRUE: {
      switch_1 = switch_2;
      break;
    }
    case FALSE: {
      switch_1 = 3;
      break;
    }
  };
  
  o = switch_1;
  
  return o;
}

void main1_init (struct main1_mem* mem) {
  ;
}

int main1 () {
  int aux__1;
  int o;
  int call_1;
  
  call_1 = m0();
  
  aux__1 = call_1;
  
  o = aux__1;
  
  return o;
}

int main (int argc, char* argv[]) {
  int res;
  
  while (1) {
    res = main1();
    
    printf("%d", res);
    
    fflush(0);
    
    usleep(333333);
  };
}
