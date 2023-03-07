#include <stdio.h>
#include <string.h>

int main()
{
    char m[3][3] = {{'7', '8', '9'}, {'4', '5', '6'}, {'1', '2', '3'}};
    int n;
    if (scanf("%d", &n) != 1) return 1;
    char out[n];
    out[n] = '\0';
    int x = 1, y = 1;
    for (int i = 0; i < n; i++){
        char str[1000];
        if (scanf("%s", str) != 1) return 1;
        for (size_t j = 0; j < strlen(str); j++){
            switch (str[j]){
            case 'C':
                if (x <= 0)
                   break;
                else x -= 1;
                break;
            case 'B':
                if (x >= 2)
                    break;
                else x += 1;
                break;
            case 'D':
                if (y >= 2)
                    break;
                else y += 1;
                break;
            case 'E':
                if (y <= 0)
                break;
                else y -= 1;
                break;
            }
        }
        out[i] = m[x][y];
    }
    printf("%s\n", out);
}