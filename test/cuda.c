#include "../common/book.h.h"
#include "../common/cpu_bitmap.h.h"
;

/**DEFINED: "SQ" (template)**/;

#define DIM 1000
;

struct cu_complex{
  float r;
float i;
};

/**DEFINED: "CU-COMPLEX-DECL" (template)**/;

float cu_complex_magnitude(cu_complex x){
   return (((((x).i)*((x).i)))+((((x).r)*((x).r))));
};

cu_complex cu_complex_mul(cu_complex x,cu_complex y){
   cu_complex z;
   (((z).r)=((((((x).r)*((y).r)))-((((x).i)*((y).i))))));
   (((z).i)=((((((x).i)*((y).r)))+((((x).r)*((y).i))))));
   return z;
};

cu_complex cu_complex_add(cu_complex x,cu_complex y){
   cu_complex z;
   (((z).r)=((((x).r)+((y).r))));
   (((z).i)=((((x).i)+((y).i))));
   return z;
};

__device__ int julia( x, y){
   const float scale=1.5;
   /**DEFINED: "JVAR" (template)**/;
   float jx=((scale)*(((int)(((((((DIM)/(2)))-(x)))/(((DIM)/(2))))))));
   float jy=((scale)*(((int)(((((((DIM)/(2)))-(x)))/(((DIM)/(2))))))));
   /**DEFINED: "CVAR" (template)**/;
   struct cu_complex c=((struct cu_complex)({-9.8, 0.156}));
   struct cu_complex a=((struct cu_complex)({jx, jy}));
   int i=0;
   for(((i)=(0));((i)<(200));++(i)){
   ((a)=(cu_complex_add(cu_complex_mul(a,a),c)));
   if(((cu_complex_magnitude(a))>(1000))) {
   return 0;
};
};
   return 1;
};

__global__ void kernel(unsigned char *ptr){
   int x=blockIdx.x;
   int y=blockIdx.y;
   int offset=((x)+(((y)*(gridDim.x))));
   int julia_value=julia(x,y);
   (((ptr)[((0)+(((offset)*(4))))])=(((255)*(julia_value))));
   (((ptr)[((1)+(((offset)*(4))))])=(0));
   (((ptr)[((2)+(((offset)*(4))))])=(0));
   (((ptr)[((3)+(((offset)*(4))))])=(255));
};

;

int main(int argc,char **argv){
   CPUbitmap bitmap(DIM,DIM);
   unsigned char *dev_bitmap;
   HANDLE_ERROR(cudaMalloc(((void**)(&(dev_bitmap))),(bitmap).image_size()));
   dim3 grid(DIM,DIM);
   kernel<<<grid,1>>>(dev_bitmap);
   HANDLE_ERROR(cudaMemcpy((bitmap).get_ptr(),dev_bitmap,(bitmap).image_size(),cudaMemcpyDeviceToHost));
   (bitmap).display_and_exit();
   HANDLE_ERROR(cudaFree(dev_bitmap));
};

