#include <stdlib.h>

#include <printf.h>

#include <unistd.h>

enum inductive_bool {
  FALSE,
  TRUE
};

struct check_mem {

  int aux__2_next5;
  int aux__4_next4;
  int aux__6_next3;
  int aux__8_next2;
  int aux__10_next1;
};

void check_init (struct check_mem* mem) {
  mem->aux__2_next5 = 1;
  
  mem->aux__4_next4 = 0;
  
  mem->aux__6_next3 = 1;
  
  mem->aux__8_next2 = 1;
  
  mem->aux__10_next1 = 0;
}

int check (struct check_mem* mem) {
  int aux__10;
  int aux__9;
  int aux__8;
  int aux__7;
  int aux__6;
  int aux__5;
  int aux__4;
  int aux__3;
  int aux__2;
  int aux__1;
  int n1;
  int n2;
  int o;
  
  aux__1 = 0;
  
  aux__2 = mem->aux__2_next5;
  
  aux__4 = mem->aux__4_next4;
  
  aux__5 = 0;
  
  aux__6 = mem->aux__6_next3;
  
  aux__7 = 0;
  
  aux__8 = mem->aux__8_next2;
  
  aux__10 = mem->aux__10_next1;
  
  n1 = aux__2 ? 0 : (aux__4 + 1);
  
  n2 = aux__6 ? 0 : aux__8 ? 1 : aux__10;
  
  o = (n1 + n2);
  
  aux__3 = n1;
  
  aux__9 = (n2 + 1);
  
  mem->aux__2_next5 = aux__1;
  
  mem->aux__4_next4 = aux__3;
  
  mem->aux__6_next3 = aux__5;
  
  mem->aux__8_next2 = aux__7;
  
  mem->aux__10_next1 = aux__9;
  
  return o;
}

int main (int argc, char* argv[]) {
  struct check_mem mem;
  int res;
  
  check_init(&(mem));
  
  if ((argc < 1)) {
    printf("Error : %d needed arguments were not provided", 0);
    
    exit(1);
  };
  
  while (1) {
    res = check(&(mem));
    
    printf("%d", res);
    
    fflush(0);
    
    sleep(1);
  };
}
