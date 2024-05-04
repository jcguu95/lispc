#include <stdio.h>
#include <stdlib.h>
;

int square(int x){
   return ((x)*(x));
};

int *map(int (*f)(int),int *arr,size_t size){
   int *result=((int*)(malloc(((size)*(sizeof(int))))));
   for(size_t i=0;((i)<(size));++(i)){
   (((result)[i])=(f((arr)[i])));
};
   return result;
};

int main(){
   int numbers[]={1, 2, 3, 4, 5};
   size_t size=((sizeof(numbers))/(sizeof((numbers)[0])));
   int *result=map(square,numbers,size);
   for(size_t i=0;((i)<(size));++(i)){
   printf("%d ",(result)[i]);
};
   printf("\n");
   return 0;
};

