#include <stdio.h>
#include <string.h>

int main(){
    int linhas,paiva,n_anoes,max = 0,soma = 0;
    if (scanf("%d",&linhas) != 1) return 1;
    int out[linhas];
    memset(out, linhas, 1);
    for(int i = 0; i < linhas ; i++){
        if (scanf("%d",&n_anoes) != 1) return 1;
        for(int j = 0 ; j < n_anoes ; j++){
            if (scanf("%d",&paiva) != 1) return 1;
            if (paiva > max) {
				soma++;
				max = paiva;
            }
        }
        out[i] = soma;
        max = 0;
        soma = 0;
    }
    for(int p = 0; p < linhas ; p++){
        printf("%d\n",out[p]);
    }
    return 0;
}
