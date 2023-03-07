#include <stdio.h>
#include <stdlib.h>
int main () {
    int n, i, cartas, movh = 0, movv = 0;
    if(scanf("%d",&n) != 1)
    abort();
    for(i=1;i<=n;i++){
       if(scanf("%d",&cartas)!=1)
       abort ();
       switch (cartas%4)
        {
        case 1:
            movv-=1;
            break;
        case 2:
            movv+=1;
            break;
        case 3:
            movh-=1;
            break;
        case 0:
            movh+=1;
            break;
        default:
            break;
        }
       }
    printf("%d %d\n",movh,movv);
}