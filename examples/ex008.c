#include <stdlib.h>

#include <printf.h>

enum inductive_bool {
  FALSE,
  TRUE
};

void check_init (struct check_mem* mem) {
  ;
}

int check (int x, int y) {
  int o;
  
  o = (x == y);
  
  return o;
}

int main (int argc, char* argv[]) {
  int argv_0;
  int argv_1;
  int res;
  
  if ((argc < 3)) {
    printf("Error : %d needed arguments were not provided", 2);
    
    exit(1);
  };
  
  argv_0 = atoi(argv[1]);
  
  argv_1 = atoi(argv[2]);
  
  while (1) {
    res = check(argv_0, argv_1);
    
    printf("%d", res);
  };
}
