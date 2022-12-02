#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

struct check_mem {
  int aux__2_next2;
  int aux__4_next1;
};

void check_init (struct check_mem* mem) {
  mem->aux__2_next2 = 1;
  
  mem->aux__4_next1 = 0;
}

int check (struct check_mem* mem, int x) {
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int o;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next2;
  
  aux__4 = mem->aux__4_next1;
  
  o = aux__2 ? x : aux__4;
  
  aux__3 = (o + 1);
  
  mem->aux__2_next2 = aux__1;
  
  mem->aux__4_next1 = aux__3;
  
  return o;
}

int main (int argc, char* argv[]) {
  struct check_mem mem;
  int argv_0;
  int res;
  
  check_init(&(mem));
  
  if ((argc < 2)) {
    printf("Error : %d needed arguments were not provided", 1);
    
    exit(1);
  };
  
  argv_0 = atoi(argv[1]);
  
  while (1) {
    res = check(&(mem), argv_0);
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
