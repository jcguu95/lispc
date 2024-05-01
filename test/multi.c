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
   printf("hello from thread %d\n",((1)+(((int)(((size_t)(x)))))));
   return null;
};

#define nthreads 12
;

int main(int argc,char **argv){
   pthread_t threads[nthreads];
   {
    i;
   int maxi=nthreads;
   for(((i)=(0));((i)<(maxi));++(i)){
   pthread_create(&((threads)[i]),null,&(threadfunc),((void*)(((size_t)(i)))));
};
};
   {
    i;
   int maxi=nthreads;
   for(((i)=(0));((i)<(maxi));++(i)){
   pthread_join((threads)[i],null);
};
};
   return 0;
};

