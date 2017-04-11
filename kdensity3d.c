#include <R.h>
#include <Rmath.h>

/* This C function only return one row of grid point with fixed y value.
*/
void kdensity3d(int *n, int *p, double *x, double *y, double *z, 
	double *gx, double *gy, double *res, double *b){
	double a1, a2, c;
	double u;
	int i,k;
	for (i = 0; i < *p; ++i)
	{
		a1 = 0.0;
		a2 = 0.0;
		for (k = 0; k < *n; ++k)
		{
			u = sqrt(pow(x[k] - gx[i],2.0)+pow(y[k]  - gy[0],2.0))/ b[0];
			c = (1/sqrt(2*M_PI))*exp(-0.5*pow(u,2.0))/ b[0];
			a1 += z[k] * c;
			a2 += c;
		}
		res[i] = a1/a2;
	}
}