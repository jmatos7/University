#include <stdio.h>

void swapM(int *x, int *y){
    int z = *x;
    *x = *y;
    *y = z;
}

int main () {
    int a=3,b =5;
    swapM(&a,&b);
    printf("%d %d\n",a,b);
}