#include <stdio.h>
#include <string.h>

int main() {
    char x[9];
    char y[1000];
    
    if (scanf("%s",x) != 1) return 1;
    if (scanf("%s",y) != 1) return 1;

    for (size_t i=0 ; i < strlen(x) ; i++){
        int p = x[i] - '1'; 

        for(size_t j = 0; p+j < strlen(y) ; j += strlen(x)){
            printf("%c",y[j+p]);
        }
    }
    printf("\n");
}