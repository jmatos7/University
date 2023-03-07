#include <stdio.h>

int fib(int i){
    int x=0,y=0,r=0;
    for(int n =0 ; n<=i ; n++){
        if(n<2){x=1;y=1;}
        else {
            r = x + y;
            if (n%2 ==0) y = r;
            else x=r;
        }
    }
    printf("%d\n",r);
}


int main (){
    fib(20);
}