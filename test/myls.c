#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <grp.h>
#include <dirent.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
;

;

;

#define True 1
;

#define False 0
;

#define EXECINFOP(X) ((X)&(((S_IXUSR)|((S_IXGRP)|(S_IXOTH)))))
;

#define CHIFELSE(P,A,B) putchar((P)?(A):(B))
;

#define BUFFER(X) (X)?True:(False)
;

#define NOP(X) X
;

typedef struct dirent dirent;
;

/**DEFINED: "INCPTR" (template)**/;

char *parse_arg(char **argv,char **search,char argp,int *slot){
   int i=0;
   while(*(argv)) {
   char **start=search;
   while(*(start)) {
   if(!(strcmp(*argv,*start))) {
     if(argp) {
   return (argv)[1];
};
  if(((*slot)<(i))) {
   ((*slot)=(i));
};
  return "TRUE";;
};
   ((start)=(&((start)[1])));
};
   ((argv)=(&((argv)[1])));
   (i)++;
};
   return NULL;
};

const char *CLASSIFY[]={"-c", "-classify", NULL};

const char *DISKUSAGE[]={"-d", "-disk-usage", NULL};

const char *LONGLISTING[]={"-l", "-long-listing", NULL};

const char *FOLLOWSYMLINKS[]={"-f", "-follow-symlinks", NULL};

const char *HUMANREADABLE[]={"-h", "-human-readable", NULL};

const char *RECURSIVE[]={"-r", "-recursive", NULL};

int atoi_if(char *c){
   if(!(c)) {
   return -1;
};
   return atoi(c);
};

#define BUFSIZE 256
;

char buffer[BUFSIZE];

void modeify( x){
   char ret[10]="drwxrwxrwx";
   int i=9;
   if(!(S_ISDIR(x))) {
   (((ret)[0])=('-'));
};
   while(((i)>=(1))) {
   if(((((x)%(2)))==(0))) {
   (((ret)[i])=('-'));
};
   ((x)/=(2));
   (i)--;
};
   for(((i)=(0));((i)<(10));++(i)){
   putchar((ret)[i]);
};
   putchar('\t');
};

int printhumansize( x){
   float size=((float)(x));
   char *prefixes[]={"B", "KB", "MB", "TB", "PB"};
   int i=0;
   while(((size)>(1024))) {
   ((size)/=(1024));
   (i)++;
};
   printf("%.2g%s\t",size,(prefixes)[i]);
};

char *stradd( const char *a, const char *b){
   char *c=malloc(((strlen(a))+((strlen(b))+(1))));
   memcpy(c,a,strlen(a));
   memcpy(((c)+(strlen(a))),b,((strlen(b))+(1)));
   return c;
};

typedef struct ll{
  struct ll *next;
void *data;
} ll;
;

void lsdir(char *path,char classify,char longlisting,char followsymlinks,char humanreadable,char recursive){
   DIR *dir=opendir(path);
   if(!(dir)) {
   return ;
};
   struct stat dirstat;
   dirent *dirinfo;
   ll *nextlistings;
   ll *tmp;
   while(((dirinfo)=(readdir(dir)))) {
   stat((dirinfo)->d_name,&(dirstat));
   printf("%-12s\t",(dirinfo)->d_name);
   if(recursive) {
   if((((((dirinfo)->d_type)==(DT_DIR)))&&((((((dirinfo)->d_type)==(DT_LNK)))||(followsymlinks))))) {
   if(((strcmp((dirinfo)->d_name,"."))&&(strcmp((dirinfo)->d_name,"..")))) {
     ((tmp)=(nextlistings));
  ((nextlistings)=(malloc(sizeof(ll))));
  (((nextlistings)->data)=(stradd(stradd(path,"/"),(dirinfo)->d_name)));
  (((nextlistings)->next)=(tmp));;
};
};
};
   if(classify) {
     CHIFELSE((((dirinfo)->d_type)==(DT_DIR)),'/',' ');
  CHIFELSE(EXECINFOP((dirstat).st_mode),'*',' ');
  CHIFELSE((((dirinfo)->d_type)==(DT_LNK)),'@',' ');;
};
   if(longlisting) {
     printf("%d\t",(dirinfo)->d_ino);
  modeify((dirstat).st_mode);
  struct passwd *uid=getpwuid((dirstat).st_uid);
  struct group *gid=getgrgid((dirstat).st_uid);
  printf("%s\t",(uid)?(uid)->pw_name:("root"));
  printf("%s\t",(gid)?(gid)->gr_name:("root"));
  if(humanreadable) {
   printhumansize((dirstat).st_size);
}else{
   printf("%d\t",(dirstat).st_size);
};
  struct tm *yearboundary;
  time_t rawtime=time(0);
  ((yearboundary)=(localtime(&(rawtime))));
  (((yearboundary)->tm_yday)=(0));
  (((yearboundary)->tm_mon)=(0));
  (((yearboundary)->tm_mday)=(0));
  (((yearboundary)->tm_sec)=(0));
  (((yearboundary)->tm_min)=(0));
  (((yearboundary)->tm_hour)=(0));
  if((((dirstat).st_mtime)<(mktime(yearboundary)))) {
   strftime(buffer,BUFSIZE,"%b %d %Y",localtime(&((dirstat).st_mtime)));
}else{
   strftime(buffer,BUFSIZE,"%b %d %H:%M",localtime(&((dirstat).st_mtime)));
};
  printf("%s ",buffer);;
};
   putchar('\n');
   while(nextlistings) {
   printf("\n%s:\n",((char**)((nextlistings)->data)));
   lsdir(((char**)((nextlistings)->data)),classify,longlisting,followsymlinks,humanreadable,recursive);
   ((nextlistings)=((nextlistings)->next));
};
};
};

int main(int argc,char **argv){
   int pos=0;
   int diskusage=-1;
   ((diskusage)=(atoi_if(parse_arg(argv,((char**)(DISKUSAGE)),True,&(pos)))));
   ;
     char classify=BUFFER(parse_arg(argv,((char**)(CLASSIFY)),False,&(pos)));
  char longlisting=BUFFER(parse_arg(argv,((char**)(LONGLISTING)),False,&(pos)));
  char followsymlinks=BUFFER(parse_arg(argv,((char**)(FOLLOWSYMLINKS)),False,&(pos)));
  char humanreadable=BUFFER(parse_arg(argv,((char**)(HUMANREADABLE)),False,&(pos)));
  char recursive=BUFFER(parse_arg(argv,((char**)(RECURSIVE)),False,&(pos)));;
   char *path=(((pos)==(((argc)-(1)))))?".":((argv)[((argc)-(1))]);
   lsdir(path,classify,longlisting,followsymlinks,humanreadable,recursive);
   return 0;
};

