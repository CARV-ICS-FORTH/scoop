#include <spu_intrinsics.h>
#include <spu_mfcio.h>


#include "common.h"

#include "tpc_common.h"
#include "tpc_spe.h"


#define SYNC()  __asm__ __volatile__ ("sync" : : : "memory");






Cplfloat filtre[ATL_tf]   ;


/***********************************************************************************************/
// type conversion functions


 int Vfloat2vint(float V,int Nv, float lambda, float Tr){
	
	float Vmax = lambda/(4.*Tr);	
	float Vgate = 2.*Vmax/(float)Nv;
	int v;
	if(V>=0.){
		v = (int)(1.E-5+ V/Vgate);
	}else{
		v = (int)(1.E-5+ (V+2*Vmax)/Vgate);
	}
	if(v<0) v=0;
	if(v>Nv) v=Nv;
	return(v);
}


/***********************************************************************************************/

 float vint2Vfloat (int v,int Nv, float lambda, float Tr)
{
	float Vmax = lambda/(4.*Tr);
	float V;
	V= Vmax * (2.*v+1.)/(1.*Nv);
	if(v>=Nv/2){
		V=V-2.*Vmax;
	}
	return(V);
}

/***********************************************************************************************/


 float vint2Vfloat_1 (int v,int Nv, float Vmax)
{
	float V;
	V= Vmax * (2.*v+1.)/(1.*Nv);
	if(v>=Nv/2){
		V=V-2.*Vmax;
	}
	return(V);
}

/***********************************************************************************************/

 float thint2THfloat(int th, int Nth, float Beamwidth){
	float Beamwidth_rad = Beamwidth*Pi/180.;
	return(Beamwidth_rad*(-.5 + (2.*th+1.)/(2.*Nth)));
}

/***********************************************************************************************/

 int THfloat2thint(float TH , int Nth, float Beamwidth){
		float Beamwidth_rad = Beamwidth*Pi/180.;
		float Thgate = Beamwidth_rad/(float)Nth;
		int th; 
		th = (int) ( .5 +(TH +Beamwidth_rad/2.)/ Thgate);
		if(th<0) th=0;
		if (th>Nth) th=Nth;
		return(th);
}	



/***********************************************************************************************/
 void X_2(int nsa, int nrec, int dim3, Cplfloat a[nsa][nrec][dim3],
        Cplfloat b[dim3][nsa][nrec]) {
    int j, k, l;

    for (j=0; j<nsa; j++) {
        for (k=0; k<nrec; k++) {
            for (l=0; l<dim3; l++) {
                b[l][j][k].re = a[j][k][l].re;
                b[l][j][k].im = a[j][k][l].im;
            }
        }
    }
}

/***********************************************************************************************/
 void X_1(int dim1, int dim2, int dim3, Cplfloat a[dim1][dim2][dim3],
        Cplfloat b[dim1][dim3][dim2]) {
    int j, k, l;

    for (j=0; j<dim1; j++) {
        for (k=0; k<dim2; k++) {
            for (l=0; l<dim3; l++) {
                b[j][l][k].re = a[j][k][l].re;
                b[j][l][k].im = a[j][k][l].im;
            }
        }
    }
}

/***********************************************************************************************/
 void X_5(int dim1, int dim2, Cplfloat a[dim1][dim2], Cplfloat b[dim2][dim1]) {
    int i, j;
    for (i=0; i<dim1; i++) {
        for (j=0; j<dim2; j++) {
            b[i][j].re = a[j][i].re;
            b[i][j].im = a[j][i].im;
        }
    }
}


/***********************************************************************************************/
 void Calc_steervect_spe(int nth, int nv, int ntt, int nsa, 
		float SubarraySpacing,
		float Lambda,
		float BeamAngle,
		float CarrierSpeed,
		float BeamWidth,
		float TargetDistance,
		float Tr,
		float theta,
        Cplfloat Steervect[nth][nv][ntt*nsa]) {

		
    int ant, rec, v, th;
    float V;
    double AngleCarrier_rad= BeamAngle*Pi/180.;

    double Kdeph = 2.*Pi/Lambda;

    double angldir, delta_dist, deph, deph_a, deph_v;
    for (th=0; th<nth; th++) {

        for (v=0; v<nv; v++) {
            V = vint2Vfloat(v, nv, Lambda, Tr);

            for (rec=0; rec<ntt; rec++) {
                for (ant=0; ant<nsa; ant++) {
                    angldir= AngleCarrier_rad + theta+ (SubarraySpacing*ant
                            - CarrierSpeed*Tr*rec)
                            *sin(AngleCarrier_rad)/TargetDistance;
                    delta_dist = -V*Tr*rec;
                    deph_a = Kdeph* SubarraySpacing*ant*cos(angldir);
                    deph_v = Kdeph*2* delta_dist;
                    deph=deph_a + deph_v;

                    Steervect[th][v][ant + nsa*rec].re = cos(deph);
                    Steervect[th][v][ant + nsa*rec].im = sin(deph);

                }
            }
        }
    }
}



/***********************************************************************************************/

 void Pulse_Comp(int nrg, Cplfloat ptrin[nrg], int tf, Cplfloat ptrfiltre[tf],
        Cplfloat ptrout[nrg-tf+1]) {

    int i, j;
    float R, I;
    for (i=0; i<nrg-tf+1; i++) {
        R = 0.0;
        I = 0.0;
        for (j=0; j<tf; j++) {
            R += ptrin[i+j].re * ptrfiltre[j].re - ptrin[i+j].im
                * ptrfiltre[j].im;
            I += ptrin[i+j].im * ptrfiltre[j].re + ptrin[i+j].re
                * ptrfiltre[j].im;
        }
        ptrout[i].re = R;
        ptrout[i].im = I;
    }
}



/***********************************************************************************************/
 void CovAvCov(int nrec, int nsa, int ntt, Cplfloat In[nsa][nrec],
        Cplfloat Out[ntt*nsa][ntt*nsa]) {

    int avlength = nrec - ntt +1;

    int r0, r1, a0, a1, l;
    Cplfloat S;
    for (a0=0; a0<nsa; a0++) {
        for (a1=0; a1<nsa; a1++) {
            for (r0=0; r0<ntt; r0++) {
                for (r1=0; r1<ntt; r1++) {
                    S.re=0.;
                    S.im=0.;
                    for (l=0; l<avlength; l++) {
                        S.re += In[a0][r0+l].re * In[a1][r1+l].re
                                + In[a0][r0+l].im * In[a1][r1+l].im;
                        S.im += In[a0][r0+l].im * In[a1][r1+l].re
                                - In[a0][r0+l].re * In[a1][r1+l].im;
                    }
                    Out[a0+nsa*r0][a1+nsa*r1].re = S.re;
                    Out[a0+nsa*r0][a1+nsa*r1].im = S.im;
                }
            }
        }
    }
}


/***********************************************************************************************/
 void Mat_Invert(int ntt, int nsa, Cplfloat In[ntt*nsa][ntt*nsa],
        Cplfloat Out[ntt*nsa][ntt*nsa]) {

    double inv[nsa*ntt][2*nsa*ntt][2];
    double pivot[2], coef[2];

    float re, im;

    int i=0, j=0, k=0, l=0;

    for (i=0; i<ntt*nsa; i++) {
        for (j=0; j<ntt*nsa; j++) {
            inv[i][j][0] = (double) In[i][j].re;
            inv[i][j][1] = (double) In[i][j].im;
            if (i == j) {
                inv[i][j+nsa*ntt][0] =1.0;
                inv[i][j+nsa*ntt][1] =0.0;
            } else {
                inv[i][j+nsa*ntt][0]=0.0;
                inv[i][j+nsa*ntt][1] =0.0;
            }
        }
    }

    for (i=0; i<nsa*ntt; i++) {
        pivot[0]=inv[i][i][0];
        pivot[1]=inv[i][i][1];

        if (pivot[0] == 0.) {
            printf("\n Pivot nul re = %f , im = %f\n", pivot[0], pivot[1]);
            return;
        }

        for (j=i; j<2*nsa*ntt; j++) {
            re = inv[i][j][0];
            im = inv[i][j][1];
            inv[i][j][0] = (re * pivot[0] + im * pivot[1])/(pivot[0] * pivot[0]
                    + pivot[1] * pivot[1]);
            inv[i][j][1] = (im * pivot[0] - re * pivot[1])/(pivot[0] * pivot[0]
                    + pivot[1] * pivot[1]);
        }

        for (k=0; k<nsa*ntt; k++) {
            if (i!=k) {
                coef[0] = inv[k][i][0];
                coef[1] = inv[k][i][1];
                for (l=i; l<2*nsa*ntt; l++) {
                    inv[k][l][0] -= (coef[0] * inv[i][l][0] - coef[1]
                            * inv[i][l][1]);
                    inv[k][l][1] -= (coef[0] * inv[i][l][1] + coef[1]
                            * inv[i][l][0]);
                }
            }
        }
    }

    for (i=0; i<nsa*ntt; i++) {
        for (j=0; j<nsa*ntt; j++) {
            Out[i][j].re = (float) inv[i][j+nsa*ntt][0];
            Out[i][j].im = (float) inv[i][j+nsa*ntt][1];
        }
    }
}


/***********************************************************************************************/
 void Calc_Filter(int ntt, int nsa, int Nv,
        Cplfloat CovInv[ntt*nsa][ntt*nsa], Cplfloat SteerVect[Nv][ntt*nsa],
        Cplfloat Fil[Nv][ntt*nsa]) {


    int i, j, length_filt;
    int v;
    Cplfloat X, Y, Z;


    Cplfloat W[50];
    length_filt =ntt*nsa;
 
        for (v=0; v<Nv; v++) {
            for (i=0; i<length_filt; i++) {
                Z.re=0.;
                Z.im=0.;
                for (j=0; j<length_filt; j++) {
                    X.re = SteerVect[v][ j].re;
                    X.im = SteerVect[v][ j].im;
                    Y.re= CovInv[j][i].re;
                    Y.im= CovInv[j][i].im;
                    Z.re+= X.re*Y.re + X.im*Y.im;
                    Z.im += -X.im*Y.re + X.re*Y.im;
                }
                W[i].re=Z.re;
                W[i].im=Z.im;
            }
            Z.re=0.;
            Z.im=0.;
            for (i=0; i<length_filt; i++) {
                X.re = SteerVect[v][ i].re;
                X.im = SteerVect[v][ i].im;
                Z.re += W[i].re*X.re -W[i].im*X.im;
            }
            for (i=0; i<length_filt; i++) {
                Fil[v][i].re= W[i].re/Z.re;
                Fil[v][i].im= W[i].im/Z.re;;
            }
        }
    
}

/***********************************************************************************************/
 void Apply_Filter(int Nrec, int Ntt, int Na, Cplfloat Sig[Nrec][Na],
        Cplfloat Fil[Ntt*Na], Cplfloat Out[Nrec-Ntt+1]) {

    int i, a, r;
    float R, I;

    for (i=0; i<Nrec-Ntt+1; i++) {
        R=0.;
        I=0.;
        for (r=0; r<Ntt; r++) {
            for (a=0; a<Na; a++) {
                R+= Sig[i+r][a].re * Fil[a+Na*r].re + Sig[i+r][a].im
                        * Fil[a+Na*r].im;
                I+= -Sig[i+r][a].re * Fil[a+Na*r].im + Sig[i+r][a].im
                        * Fil[a+Na*r].re;
            }
        }
        Out[i].re = R;
        Out[i].im = I;
    }
}

/***********************************************************************************************/
 void Int_Dop(int nv, int ndop, Cplfloat ptrin1[nv][ndop],
        Cplfloat ptrin2[nv][ndop],        Cplfloat ptrout[nv]) {
    int i, j;
    float R, I, X, Y;

    for (i=0; i<nv; i++) {
        R = 0.0;
        I = 0.0;
        for (j=0; j<ndop; j++) {
            X= ptrin1[i][j].re * ptrin2[i][j].re + ptrin1[i][j].im
                    * ptrin2[i][j].im;
            Y= -ptrin1[i][j].re * ptrin2[i][j].im + ptrin1[i][j].im
                    * ptrin2[i][j].re;
            R+= X;
            I+=Y;
        }
        ptrout[i].re = R;
        ptrout[i].im = I;
    }
}

/***********************************************************************************************/
 void MaxPower(int Nth, Cplfloat In[Nth], Cplfloat Out[Nth]) {

    double Pow, PowMax;
    int thmax;
    int th;
    thmax=0;

    PowMax=0.;
    for (th=0; th<Nth; th++) {
        Pow = In[th].re*In[th].re+In[th].im*In[th].im;
        if (Pow>PowMax) {
            PowMax=Pow;
            thmax=th;
        }
    }
    for (th=0; th<Nth; th++) {
        if (th!=thmax) {
            Out[th].re=0.;
            Out[th].im=0.;
        } else {
            Out[th].re=In[th].re;
            Out[th].im=In[th].im;
        }
    }
}

/***********************************************************************************************/
 void CorrecV(int Nth, int Nv, Cplfloat In[Nth][Nv], Cplfloat Out[Nth][Nv],
        float Beamwidth, float AngleCarrier, float Tr,
        float lambda, float Vcarrier) {

    int v, th, vcor;
    float AngleCarrier_rad, V, TH, dVcar;
    AngleCarrier_rad=AngleCarrier*Pi/180.;

    memset(Out, 0, Nth*Nv*sizeof(Cplfloat));

    for (th=0; th<Nth; th++) {
        TH = thint2THfloat(th, Nth, Beamwidth);
        TH += AngleCarrier_rad;
        dVcar= Vcarrier*cos(TH);
        for (v=0; v<Nv; v++) {
            V= vint2Vfloat(v, Nv, lambda, Tr);
            V -= dVcar;

            vcor = Vfloat2vint(V, Nv, lambda, Tr);

            Out[th][vcor].re = In[th][v].re;
            Out[th][vcor].im = In[th][v].im;
        }
    }
}

/***********************************************************************************************/

 double Reprodconj(double XR, double XI,double YR, double YI){
	return( XR*YR +XI*YI);
}

 double ModSquare (Cplfloat Z){
	return(Reprodconj((double)Z.re, (double)Z.im,(double) Z.re,(double)Z.im));
}


/***********************************************************************************************/
 void tfac(int Nth, int Nrg, int Nv, Cplfloat In[Nth][Nrg][Nv], int tfac_L_V,
        int tfac_L_RG, float tfac_K, float tfac_AVPWK,
        Cplfloat TOut[Nth][Nrg-tfac_L_RG+1][Nv-tfac_L_V+1]) {

    int midRg, midV, nb_neighb;
    double AvPower;
    float val;
    int th, v, rg, xv, xrg;
    double S, Sc, T;
    double sqrAvPow;
    int IsMax;

    midRg= (tfac_L_RG - 1)/2;
    midV= (tfac_L_V - 1)/2;
    nb_neighb = tfac_L_RG*tfac_L_V-1;

    AvPower= 0.;

    for (th=0; th<Nth; th++) {
        AvPower = (double)(In[th][0][0].re);
        sqrAvPow = sqrt(AvPower);
        for (rg=0; rg<=Nrg - tfac_L_RG; rg++) {
            for (v=0; v<=Nv -tfac_L_V; v++) {
                Sc=ModSquare(In[th][rg+midRg][v+midV]);
                IsMax=1;
                S=0.;
                for (xv=0; xv< 2*midV+1; xv++) {
                    for (xrg=0; xrg<2*midRg+1; xrg++) {
                        if ( (xrg!=midRg)||(xv!=midV)) {
                            T=ModSquare(In[th][rg+xrg][v+xv]);
                            if (Sc<T) {
                                IsMax=0;
                            }
                            S+= T;
                        }
                    }
                }

                if ((IsMax)&&(Sc>tfac_AVPWK* AvPower) &&(Sc> tfac_K*S
                        /(double)nb_neighb)) {
                    val = 1.*IsMax;

                } else {
                    val=0.;
                }
                TOut[th][rg][v].re=val*In[th][rg+midRg][v+midV].re/sqrAvPow;
                TOut[th][rg][v].im=val*In[th][rg+midRg][v+midV].im/sqrAvPow;
            }
        }
    }
}

/***********************************************************************************************/



 void tfac_add_edges(int Nth, int Nrg, int Nv, Cplfloat In[Nth][Nrg][Nv],
        Cplfloat Pow[Nth], int NrgLarge, int NvLarge,
        Cplfloat TOut[Nth][NrgLarge][NvLarge]) {

    int th, rg, v;
    int sidesv, sidesRg;
    float sqpow;
    sidesv = (NvLarge-Nv)/2;
    sidesRg = (NrgLarge-Nrg)/2;

    for (th=0; th<Nth; th++) {
        sqpow = (float) sqrt(Pow[th].re);
        for (rg=0; rg<NrgLarge; rg++) {
            for (v=0; v<NvLarge; v++) {
                if ( (rg<sidesRg) ||(rg>=Nrg+sidesRg) ||(v<sidesv) ||(v>=Nv
                        +sidesv)) {
                    TOut[th][rg][v].re = sqpow;
                    TOut[th][rg][v].im = 0;
                } else {
                    TOut[th][rg][v].re = In[th][rg-sidesRg][v-sidesv].re;
                    TOut[th][rg][v].im = In[th][rg-sidesRg][v-sidesv].im;
                }
            }
        }
    }
}





/***********************************************************************************************/

int execute_task(queue_entry_t *ex_task, tpc_spe_task_state_t *task_info)
{
    int exit = 0;
    void *arg0, *arg1, *arg2, *arg3;

    float *ftmp0, *ftmp1, *ftmp3;
    int *itmp2, *itmp3;

   
    switch(ex_task->funcid)
    {
         case 0:       
                        break;

  
		/* This case of  tap filter init */
         case 1:

		arg0 = ( void *) task_info->ls_addr;

                memcpy( filtre, arg0,  ex_task->arguments[0].size);

		break;

         case 2: /* Pulse_Comp */

                arg0 = ( void *) task_info->ls_addr;
                ftmp0 = arg0;

                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                ftmp1 = arg1;


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                Pulse_Comp( itmp2[0],  arg1, itmp2[1], filtre, arg0 );

		break;

         case 3: /* X1 */

		arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;


                X_1( itmp2[0], itmp2[1], itmp2[2], arg1, arg0);

		break;

         case 4: /* X2 */

		arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;


                X_2( itmp2[0], itmp2[1], itmp2[2], arg1, arg0);


		break;



         case 5: /* CovAvCov */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                
                CovAvCov(itmp2[2], itmp2[1], itmp2[3], arg1, arg0);

		break;


         case 6: /* CovAvCov */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                Mat_Invert( itmp2[0], itmp2[1], arg1, arg0 ) ;
                

		break;



         case 7: /* Calc_filter */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                arg3 = (((void *)arg2) + ex_task->arguments[2].size);
                itmp3 = arg3;


	        Calc_Filter(itmp3[0], itmp3[1], itmp3[2], arg1, arg2, arg0);

		break;

         case 8: /* Calc_filter */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                arg3 = (((void *)arg2) + ex_task->arguments[2].size);
                itmp3 = arg3;

                Apply_Filter( itmp3[0], itmp3[1], itmp3[2], arg1, arg2, arg0 );
		break;


         case 9: /* Int_Dop */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                arg3 = (((void *)arg2) + ex_task->arguments[2].size);
                itmp3 = arg3;

                Int_Dop(itmp3[0], itmp3[1] , arg1, arg2, arg0);

		break;

         case 10: /* MaxPower */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                MaxPower( itmp2[0], arg1, arg0);
                

		break;


         case 11: /* MaxPower */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                X_5( itmp2[0], itmp2[1], arg1, arg0);
                

		break;


         case 12: /* correcV */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                arg3 = (((void *)arg2) + ex_task->arguments[2].size);
                ftmp3 = arg3;

                CorrecV( itmp2[0], itmp2[1], arg1, arg0, ftmp3[0], ftmp3[1], ftmp3[2], ftmp3[3], ftmp3[4] );
                

		break;

         case 13: /* tfac */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);


                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;

                arg3 = (((void *)arg2) + ex_task->arguments[2].size);
                ftmp3 = arg3;

                
                tfac(2, itmp2[0], itmp2[1], arg1, itmp2[2],itmp2[3], ftmp3[0], ftmp3[1], arg0);

		break;

         case 14: /* tfac add adges */

                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                arg2 = (((void *)arg1) + ex_task->arguments[1].size);

                arg3 = (((void *)arg2) + ex_task->arguments[2].size);
                itmp2 = arg3;

                tfac_add_edges(1, itmp2[0], itmp2[1], arg1, arg2, itmp2[2], itmp2[3], arg0);

		break;


         case 15: /* Calc_streervect */
                arg0 = ( void *) task_info->ls_addr;
                arg1 = (((void *)arg0) + ex_task->arguments[0].size);
                ftmp1 = arg1;

                arg2 = (((void *)arg1) + ex_task->arguments[1].size);
                itmp2 = arg2;
                Calc_steervect_spe(1, itmp2[0], itmp2[1], itmp2[2],  ftmp1[0], ftmp1[1], ftmp1[2], ftmp1[3], ftmp1[4], ftmp1[5], ftmp1[6], ftmp1[7], arg0);
                break; 

         default:
                 exit = 1;
		 fprintf(stderr, ">>>>>>>>>>> EXIT: %d\n", ex_task->funcid  );
	 	 SYNC();
                 break;
    }


    task_info->state = EXECUTED;
    return exit;
}

