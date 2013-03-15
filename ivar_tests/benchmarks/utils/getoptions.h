#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
/*
 * Function to evaluate argv[]. specs is a 0 terminated array of command 
 * line options and types an array that stores the type as one of 
 */
#define INTARG 1
#define DOUBLEARG 2
#define LONGARG 3
#define BOOLARG 4
#define STRINGARG 5
#define BENCHMARK 6
/*
 * for each specifier. Benchmark is specific for cilk samples. 
 * -benchmark or -benchmark medium sets integer to 2
 * -benchmark short returns 1
 * -benchmark long returns 2
 * a boolarg is set to 1 if the specifier appears in the option list ow. 0
 * The variables must be given in the same order as specified in specs. 
 */

//void get_options(int argc, char *argv[], char *specs[], int *types,...);
void get_options(int argc, char *argv[], char *specs[], int *types,...)
{
     va_list ap;
     int type, i;
     int *intval;
     double *doubleval;
     long *longval;
     char *stringval;

     va_start(ap, types);

     while (((type = *types++) != 0) && (specs != 0)) {
	  switch (type) {
	      case INTARG:
		   intval = (int *) va_arg(ap, int *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *intval = atoi(argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case DOUBLEARG:
		   doubleval = (double *) va_arg(ap, double *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *doubleval = atof(argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case LONGARG:
		   longval = (long *) va_arg(ap, long *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *longval = atol(argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case BOOLARG:
		   intval = (int *) va_arg(ap, int *);
		   *intval = 0;
		   for (i = 1; i < argc; i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *intval = 1;
			     argv[i][0] = 0;
			}
		   break;
	      case STRINGARG:
		   stringval = (char *) va_arg(ap, char *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     strcpy(stringval, argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case BENCHMARK:
		   intval = (int *) va_arg(ap, int *);
		   *intval = 0;
		   for (i = 1; i < argc; i++) {
			if (!(strcmp(argv[i], specs[0]))) {
			     *intval = 2;
			     if ((i + 1) < argc) {
				  if (!(strcmp(argv[i + 1], "short")))
				       *intval = 1;
				  if (!(strcmp(argv[i + 1], "medium")))
				       *intval = 2;
				  if (!(strcmp(argv[i + 1], "long")))
				       *intval = 3;
				  argv[i + 1][0] = 0;
			     }
			     argv[i][0] = 0;
			}
		   }
		   break;
	  }
	  specs++;
     }
     va_end(ap);

     for (i = 1; i < argc; i++)
	  if (argv[i][0] != 0)
	       printf("\nInvalid option: %s\n", argv[i]);

}
