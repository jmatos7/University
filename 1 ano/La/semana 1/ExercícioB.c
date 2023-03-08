#include <stdio.h>
int main()
{
	int x1,x2,x3;
	if (scanf("%d",&x1)!=1){
			return 1;
		}
	if (scanf("%d",&x2)!=1){
			return 1;
		}
	if (scanf("%d",&x3)!=1){
			return 1;
		}
		
	if (x1<=x2 && x2<=x3){
		printf("OK\n");
	}
	else if (x1>=x2 && x2>=x3){
		printf("OK\n");
	}
	else {
		printf("NAO\n");
	}
	
	
}