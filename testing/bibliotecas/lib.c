#include "lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/random.h>
#include <string.h>
#include<time.h>
int getSize() {
  return 20;
}

int getRandomInteger(int rangoInferior, int rangoSuperior){
  int rangoFinal = rangoSuperior - rangoInferior;
  time_t t;
  // Inicializando generador de numeros random
  srandom(time(NULL));

  int numero_aleatorio = (int)random()%rangoFinal + rangoInferior;
  return numero_aleatorio;
}
// int main(){
//   int num = getRandomInteger(1, 4);
//   printf("%d\n", num);
// }
