int num[8];
int num[8][16];
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
	i = (1 + 1)*2;
	
	for(i = 0; i < 8; i = i+1){
	  print(num[i]);
	}
}

void bubble_sort() {
	int improve, i, temp;
	improve = 1;
	
	while( improve != 0 ){
		improve = 0;
		for(i = 0; i < 7; ++i){
			if( num[i] > num[i+1] ) {
				temp = num[i];
				num[i] = num[i+1];
				num[i+1] = temp;
				
				improve = improve + 1;
			}
		}
	}
}

int *hoge( int art, int *banana){
  int *s, t, *u;
  float p;
  int x;

  m = ++p;

  return x+2;

}
