#include <stdio.h>

/*    iNZEL CMD
Pedra 	@ 	*
Papel 	| 	-
Tesoura X 	+
*/
int main(){
    int vitoria_Inzel= 0,vitoria_CMD = 0,draw = 0;
    int jogadas, i;
    char inzel,CMD;
    
    if(scanf("%d\n",&jogadas) != 1) return 1;
    
    for(i=0;i<jogadas;i++){
        if(scanf("%c",&inzel)!= 1) return 1;
        if(scanf("%c\n",&CMD )!= 1) return 1;
       switch (inzel) {
            case '@':
                switch (CMD) {
                    case '*':
                        draw++;
                        break;
                    case '-':
                        vitoria_CMD++;
                        break;
                    case '+':
                        vitoria_Inzel++;
                        break;
                }   
                break;
            case '|':
                switch (CMD) {
                    case '*':
                        vitoria_Inzel++;
                        break;
                    case '-':
                        draw++;
                        break;
                    case '+':
                        vitoria_CMD++;
                        break;
                }
                break;
            case 'X':
                switch(CMD) {
                    case '*':
                        vitoria_CMD++;
                        break;
                    case '-':
                        vitoria_Inzel++;
                        break;
                    case '+':
                        draw++;
                        break;
                }
                break;
        }
    }
    printf("%d %d %d\n",vitoria_Inzel, vitoria_CMD, draw);
}