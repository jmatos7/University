#include <stdio.h>

int main(){
    int andar,max_andar,comandos,i;
    int comando;

    if (scanf("%d", &andar)!= 1) return 1; 
    if (scanf("%d", &max_andar)!= 1) return 1; 
    if (scanf("%d", &comandos)!= 1) return 1;

    for (i = 0; i < comandos; i++) {
        if (scanf("%d", &comando) != 1) return 1;
        switch (comando) {
            case 1:
                if (andar < max_andar) {
                    andar++;
                }
                break;
            case -1:
                if (andar > 0) {
                    andar--;
                }
                break;
        }
    }

    printf("%d\n", andar);

    return 0;
}