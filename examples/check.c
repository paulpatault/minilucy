/********
* ec2c version 0.68
* c file generated for node : check 
* context   method = HEAP
* ext call  method = PROCEDURES
********/
#include <stdlib.h>
#include <string.h>
#define _check_EC2C_SRC_FILE
#include "check.h"
/*--------
Internal structure for the call
--------*/
typedef struct  {
   void* client_data;
   //INPUTS
   _integer _x;
   //OUTPUTS
   _integer _o;
   //REGISTERS
   _integer M4;
   _boolean M4_nil;
   _boolean M2;
} check_ctx;
/*--------
Output procedures must be defined,
Input procedures must be used:
--------*/
void check_I_x(check_ctx* ctx, _integer V){
   ctx->_x = V;
}
extern void check_O_o(void* cdata, _integer);
#ifdef CKCHECK
extern void check_BOT_o(void* cdata);
#endif
/*--------
Internal reset input procedure
--------*/
static void check_reset_input(check_ctx* ctx){
   //NOTHING FOR THIS VERSION...
}
/*--------
Reset procedure
--------*/
void check_reset(check_ctx* ctx){
   ctx->M4_nil = _true;
   ctx->M2 = _true;
   check_reset_input(ctx);
}
/*--------
Copy the value of an internal structure
--------*/
void check_copy_ctx(check_ctx* dest, check_ctx* src){
   memcpy((void*)dest, (void*)src, sizeof(check_ctx));
}
/*--------
Dynamic allocation of an internal structure
--------*/
check_ctx* check_new_ctx(void* cdata){
   check_ctx* ctx = (check_ctx*)calloc(1, sizeof(check_ctx));
   ctx->client_data = cdata;
   check_reset(ctx);
   return ctx;
}
/*--------
Step procedure
--------*/
void check_step(check_ctx* ctx){
//LOCAL VARIABLES
   _integer L1;
   _integer L5;
   _integer T4;
//CODE
   if (ctx->M2) {
      L1 = ctx->_x;
   } else {
      L1 = ctx->M4;
   }
   check_O_o(ctx->client_data, L1);
   L5 = (L1 + 1);
   T4 = L5;
   ctx->M4 = T4;
   ctx->M4_nil = _false;
   ctx->M2 = ctx->M2 && !(_true);
}
