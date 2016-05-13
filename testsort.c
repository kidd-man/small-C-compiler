int num[8];
int **ababababa[8][16];
int **k, *l, m;
int **hoge(int a, int *b);

void bubble_sort();

int main(){
        int i;
        num[0] = 1;
        num[1] = 1;
        num[2] = 1;
        num[3] = 1;
        num[4] = 1;
        num[5] = 1;
        num[6] = 1;
        num[7] = 5;
	
        bubble_sort();
	
        for(i = 0; i < 8; i = i+1){
	  print(num[i]);
        }
}

void bubble_sort() {
	int improve, i, temp;
	improve = 1;
	
	while( improve != 0 ){
		improve = 0;
		for(i = 0; i < 7; i++){
			if( num[i] > num[i+1] ) {
				temp = num[i];
				num[i] = num[i+1];
				num[i+1] = temp;
				
				improve = improve + 1;
			}
		}
	}
}

int *hoge( int art, int *banana ){
    int a, b, c;
    float p;
    int x;
    a = 1;
    b = 2;
    c = 3;

    x = (a,b) + 1;
    x = a,b;
    x = (a+=1, a+b);
    x = a+=1,a+b;
    x = a,b,c;
    x = (a,b,c);
    x = ++a;
    x = a++;
    x += ++a;
    x += a++;
    if(x == 1){
    
    }else{
    
    }
    return x+2;

}
