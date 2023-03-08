#include <stdio.h>
int main() {
    int altura1, altura2, altura3,menor,maior,meio;
    if (scanf("%d", &altura1)!=1){
    	return 1;
    }
    if (scanf("%d", &altura2)!=1){
    	return 1;
    }
    if (scanf("%d", &altura3)!=1){
    	return 1;
    }
    if (altura1 <= altura2 && altura1 <= altura3){
        menor = altura1;
        if (altura2 <= altura3) {
            meio = altura2;
            maior = altura3;
        } else {
            meio = altura3;
            maior = altura2;
        }
    } else if (altura2 <= altura1 && altura2 <= altura3) {
        menor = altura2;
        if (altura1 <= altura3) {
            meio = altura1;
            maior = altura3;
        } else {
            meio = altura3;
            maior = altura1;
        }
    } else {
        menor = altura3;
        if (altura1 <= altura2) {
            meio = altura1;
            maior = altura2;
        } else {
            meio = altura2;
            maior = altura1;
        }
    }
    printf("%d %d %d\n",menor,meio,maior);
    return 0;
}
