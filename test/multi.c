#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
;

/**DEFINED: "LOOP-N" (template)**/;

/**DEFINED: "VOIDINT" (template)**/;

/**DEFINED: "INTVOID" (template)**/;

void *threadfunc(void *x){
   sleep(0);
   int i=((int)(((size_t)(x))));
   printf("Hello from thread %d\n",((1)+(((int)(((size_t)(x)))))));
   return NULL;
};

#define NTHREADS 12
;

int main(int argc,char **argv){
   pthread_t threads[NTHREADS];
   {
    i;
   int maxi=NTHREADS;
   for(((i)=(0));((i)<(maxi));++(i)){
   pthread_create(&((threads)[i]),NULL,&(threadfunc),((void*)(((size_t)(i)))));
};
};
   {
    i;
   int maxi=NTHREADS;
   for(((i)=(0));((i)<(maxi));++(i)){
   pthread_join((threads)[i],NULL);
};
};
   return 0;
};

