#include <stdio.h>

int main() {
    int i, biscoitos, max_biscoitos = 0,soldado = 0;
    
    for (i = 1; i <= 5; i++) {
        if (scanf("%d", &biscoitos) !=1){
           return 1;
        }
        else {
        if (biscoitos > max_biscoitos) {
             max_biscoitos = biscoitos;
            soldado = i;
        }
    }
    }
    printf("%d\n", soldado);
    
    return 0;
}
