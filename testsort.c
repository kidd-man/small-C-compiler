int num[8];

int num[8][16];

int **k, *l, m;

int **hoge(int a, int *b);
void bubble_sort();

int main(){
	int i;
	num[0] = 4;
	num[1] = 1;
	num[2] = 6;
	num[3] = 5;
	num[4] = 9;
	num[5] = 7;
	num[6] = 2;
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
		for(i = 0; i < 7; i = i+1){
			if( num[i] > num[i+1] ) {
				temp = num[i];
				num[i] = num[i+1];
				num[i+1] = temp;
				
				improve = improve + 1;
			}
		}
	}
}

int *hoge(int art, int *banana){
	int *s,t,*u;
	float p;
	int x;
	if ((1+1==2) || (2+2!=4) && (3+3>=6)){
		return &art;
	}else{
		return 2*3;
	}
	
	for(x = 1; x <= 1; x=x+1) return banana;
}