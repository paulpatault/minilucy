#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

void check_init (struct check_mem* mem) {
  ;
}

int check (int x, int y, float z) {
  int aux__1;
  int o;
  int call_1;
  
  o = (x == y);
  
  printf("%d", x);
  
  aux__1 = call_1;
  
  return o;
}

int main (int argc, char* argv[]) {
  int argv_0;
  int argv_1;
  float argv_2;
  int res;
  
  if ((argc < 4)) {
    printf("Error : %d needed arguments were not provided", 3);
    
    exit(1);
  };
  
  argv_0 = atoi(argv[1]);
  
  argv_1 = atoi(argv[2]);
  
  argv_2 = atof(argv[3]);
  
  while (1) {
    res = check(argv_0, argv_1, argv_2);
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
