#include <stdio.h>
;

#include <stdlib.h>
;

typedef struct linked_list_node linked_list_node;
;

typedef struct linked_list linked_list;
;

struct linked_list_node{
  linked_list_node *prev;
linked_list_node *next;
void *data;
};


/********************************************/
/* this is where the linked list is defined */
/********************************************/

;

struct linked_list{
  linked_list_node *first;
linked_list_node *last;
 length;
};

linked_list_node *new_linked_list_node(linked_list_node *prev,linked_list_node *next,void *data){
   linked_list_node *retval;
   ((retval)=(malloc(sizeof(linked_list_node))));
   (((retval)->prev)=(prev));
   (((retval)->next)=(next));
   (((retval)->data)=(data));
   return retval;
};

linked_list *new_linked_list(){
   linked_list *retval;
   ((retval)=(malloc(sizeof(linked_list))));
   (((retval)->first)=(((retval)->last)=(new_linked_list_node(null,null,null))));
   (((retval)->length)=(0));
   return retval;
};

void map_linked_list(linked_list *list,void *void*;
void*(*func)(),void *etc){
   linked_list_node *cursor;
   for(((cursor)=((list)->first));cursor;((cursor)=((cursor)->next))){
   (*(func))((cursor)->data,etc);
};
};

void **listify_linked_list(linked_list *list);

void **listify_linked_list_(linked_list_node *curr, n);


/**********************************/
/* produces null_terminated list. */
/**********************************/

;

void **listify_linked_list(linked_list *list){
   return listify_linked_list_((list)->first,0);
};

void **listify_linked_list_(linked_list_node *curr, n){
   void **ret;
   
/* allocate ret as appropriate, then set it. */

;
   if(curr) {
     ((ret)=(listify_linked_list_((curr)->next,((n)+(1)))));
  (((ret)[n])=(null));;
}else{
     ((ret)=(malloc(((n)*(sizeof(void*))))));
  (((ret)[n])=((curr)->data));;
};
   return ret;
};


/********************/
/* deletes the list */
/********************/

;

void *delete_linked_list(linked_list *list,char free_data);

void *delete_linked_list_(linked_list_node *curr,char free_data);

void *delete_linked_list(linked_list *list,char free_data){
   delete_linked_list_((list)->first,free_data);
};

void *delete_linked_list_(linked_list_node *curr,char free_data){
   if(free_data) {
   free((curr)->data);
};
   delete_linked_list_((curr)->next,free_data);
   free(curr);
};

#define arrlen 12
;

/**DEFINED: "SECOND" (template)**/;


/********/
/* main */
/********/

;

int main(int argc,char **argv){
   float x[arrlen];
    i;
   (((x)[2])=(23.45f));
   printf("currently, the second value of x is %f\n",(x)[2]);
   for(((i)=(0));((i)<(arrlen));++(i)){
   (((x)[i])=(((((float)(i)))/(2))));
   printf("%f %d\n",(x)[i],i);
};
   return 0;
};

