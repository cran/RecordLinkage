/* ========================================================================== */
/*                                                                            */
/*   Filename.c                                                               */
/*   (c) 2001 Andreas Borg                                                    */
/*                                                                            */
/*   Adaption des Algorithmus AS 207 (Fitting eines log-linearen Modell       */
/*   nach: Michael Haber, Algorithm AS 207: Fitting a General Log-Linear      */
/*         Model, in: Applied Statistics 33 Vol. 33 No. 3 (1984),             */
/*         S. 358-362                                                         */
/*																																						*/
/*   Erweiterung um maximale Anzahl an Iterationen übernommen von:						*/
/*	 David Duffy (2006). gllm: Generalised log-linear model. R package				*/
/*   version 0.31.																														*/
/*                                                                            */
/* ========================================================================== */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <R.h>
#include <R_ext/Utils.h>


/**
 * Fit Log-Linear Model to observed contingency table y.
 * @param y Observed contingencies. Array of size J. 
 * @param s Scatter matrix. s[i] is the cell in the observed array that
 *          corresponds to cell i in the full array. Array of size I.
 * @param C Design matrix. Array of size I*(K+1), of which the actual matrix
 *          takes up I*K cells while the last I are needed for normalization.
 * @param maxit Maximum number of iterations.
 * @param tol Convergence parameter.
 * @param E Full contingency table. Should be initialized with either ones or
 *          a priori estimates. Array of size I
 * @param I Number of cells in the full table.
 * @param J Number of cells in the observed table.
 * @param K Number of columns in the design matrix.
 * @return Fitted full contingency table in E.
 */                                    
void mygllm (int * y, int * s, double * C, int * maxit, double * tol, double * E, 
          int * I, int * J, int * K)
{
  /* Zählvariablen */
  unsigned int i;
  unsigned int j;
  
  /* Speicherplatz */
  
  double * X = (double *) R_alloc(*I, sizeof(double));
  double * E_alt = (double *) R_alloc(*I, sizeof(double));
  
  /* initialisiert geschätzte beobachtete Werte auf 0 */
 double * F= (double *) R_alloc(*J,sizeof(double));

  /* Zwischenspeicher für F */
 double * F_alt= (double *) R_alloc(*J,sizeof(double));
 for (j=0;j<*J;j++)
   F_alt[j]=0;
  
  /* Designmatrix normalisieren */
  /* maximale Zeilensumme bestimmen */
  double max_sum=0;
  double sum;
  double * sums;
  sums = (double *) R_alloc(*I,sizeof(double));
 
  for (i=0; i<*I; i++)
  {
    sum=0;
    for (j=0; j<*K; j++)
      sum+=C[i+j* *I];   // Indizierung: Zeile + Spalte * Zeilenanzahl
    sums[i]=sum; // Jede Zeilensumme speichern, wird noch benötigt
    if (sum > max_sum)
      max_sum=sum;
  }
  

  /* Normierung: Teilen durch maximale Zeilensumme */
  if (max_sum!=1)
    for (i=0; i<*I * *K; i++)
      C[i]/=max_sum;   // Indizierung: Zeile + Spalte * Zeilenanzahl

   
  /* Normierung: Alle Zeilensummen auf 1 */
  j=*I * *K; // Anfangsindex der letzten Spalte
  
  for (i=0;i<*I;i++)
  {
    C[j+i]=1-sums[i]/max_sum; // vorher erfolgte Normierung wird eingerechnet 
  }
  
  int KK = *K+1;

//   print2Ddoublearray(C,*I,KK);
//   printf("\n");
  /* nach der Normierung Speicherplatz reservieren */
  double * Z = (double *) R_alloc(KK,sizeof(double));
  double * G = (double *) R_alloc(KK,sizeof(double));
  
  /* eigentlicher Algorithmus beginnt hier */
  int it = 0;  // Zählt Anzahl der Iterationen
  while (it<*maxit)
  {
    /* allow interruption through CTRL-C */
    R_CheckUserInterrupt();
    it++;
    //Rprintf("Iteration %d\n", it);
    /* Aufsummieren der geschätzten beobachtbaren Häufigkeiten in F */
    for (i=0; i<*J; i++) 
      F[i]=0.0;
    for (i=0; i<*I; i++)
      F[s[i]]+=E[i];
      
    
    /* Konvergenzkriterium überprüfen */
    
    int break_flag = 1;
    for (i=0; i<*J; i++)
    {
      if (fabs(F[i]-F_alt[i]) > *tol)
      {
        break_flag=0;
        break;
      } 
    }
    if (break_flag)
      break;

    /* Zwischenspeichern der alten Werte in F */    
    for (i=0; i<*J; i++)
      F_alt[i]=F[i];
      
    /* x_i=(e_i * y_ji)/f_ji */
    for (i=0;i<*I;i++)
    {
      if (F[s[i]]!=0)
        X[i]=E[i] * y[s[i]] / F[s[i]];
    }    
    /* z_k=... */
    int k;
    for (k=0;k<KK;k++)
    {
      Z[k]=0;
      for (i=0;i<*I;i++)
        Z[k]+=C[i+k * *I]*X[i];
    }
    do
    {
      for (i=0;i<*I;i++)
        E_alt[i]=E[i];
      for (k=0;k<KK;k++)
      {
        G[k]=0;
        for (i=0;i<*I;i++)
          G[k]+=C[i+k * *I] * E[i];
      /* e_i^*=... */
        for (i=0;i<*I;i++)
          if (G[k]!=0 && C[i+k * *I]!=0)
                E[i]*=pow(Z[k]/G[k],C[i+k * *I]);
      }
      /* Konvergenzkriterium überprüfen */
      break_flag=0;
      for (i=0;i<*I;i++)
      {  
        if (fabs(E[i]-E_alt[i]) > *tol)
        {
          break_flag=1;
          break; 
        }
      }
    } while (break_flag);
  }
}
