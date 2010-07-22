/*****************************************************************************/
// Source File: stap_bench_trans.c
// (Copyright © THALES 2010 All rights reserved) 
// THE PROGRAM IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
// OF ANY KIND, EITHER EXPRESS OR IMPLIED INCLUDING, WITHOUT LIMITATION, 
// ANY WARRANTIES ON ITS, NON-INFRINGEMENT, MERCHANTABILITY, SECURED, 
// INNOVATIVE OR RELEVANT NATURE, FITNESS FOR A PARTICULAR PURPOSE OR 
// COMPATIBILITY WITH ANY EQUIPMENT OR SOFTWARE.
//
// Authors: 	Eric Lenormand (eric.lenormand@thalesgroup.com),
//		Remi Barrere (remi.barrere@thalesgroup.com),
//	   	Sami Yehia (sami.yehia@thalesgroup.com)
//	   	Teodora Petrisor (claudia-teodora.petrisor@thalesgroup.com)
//
// Date: Jan,8,2010
//
// -----------------------------------------------------------------------------
// TPC Cell runtime port: Michail Alvanos alvanos@ics.forth.gr
//
/******************************************************************************/

#include <ppu_intrinsics.h>
#include <getopt.h>

#include "common.h"
#include "tpc_ppe.h"
#include "tpc_common.h"


    Cplfloat filtre[ATL_tf]  ;
    Cplfloat Steervect[ATL_Nth][ATL_Nv][ATL_ntt*ATL_nsa];
    Cplfloat tab_coh[ATL_Nv][ATL_nrec-ATL_ntt+1];
    Cplfloat out_pulse[ATL_nsa][ATL_nrec][ATL_Nrgf]   ;
    Cplfloat in_apply[ATL_Nrgf][ATL_nrec][ATL_nsa];
    Cplfloat in_cov[ATL_Nrgf][ATL_nsa][ATL_nrec];
    Cplfloat out_avcov[ATL_Nrgf][ATL_ntt*ATL_nsa][ATL_ntt*ATL_nsa];
    Cplfloat mat_inv[ATL_Nrgf][ATL_ntt*ATL_nsa][ATL_ntt*ATL_nsa];
    Cplfloat Fil[ATL_Nrgf][ATL_Nth][ATL_Nv][ATL_ntt*ATL_nsa];
    Cplfloat out_apply[ATL_Nrgf][ATL_Nth][ATL_Nv][ATL_nrec-ATL_ntt+1];
    Cplfloat out_int_dop[ATL_Nrgf][ATL_Nth][ATL_Nv];
    Cplfloat in_avpow[ATL_Nth][ATL_Nrgf][ATL_Nv];
    Cplfloat out_avpow[ATL_Nth];
    Cplfloat out_facedges[ATL_Nth][ATL_Nrg_enlarged][ATL_Nv_enlarged];
    Cplfloat out_face1[ATL_Nth][ATL_Nrgf][ATL_Nv];
    Cplfloat in_maxpow[ATL_Nrgf][ATL_Nv][ATL_Nth];
    Cplfloat out_maxpow[ATL_Nrgf][ATL_Nv][ATL_Nth];
    Cplfloat in_correctionV[ATL_Nrgf][ATL_Nth][ATL_Nv];
    Cplfloat out_correctionV[ATL_Nrgf][ATL_Nth][ATL_Nv];



/***********************************************************************************************/

double Reprodconj(double XR, double XI,double YR, double YI){
	return( XR*YR +XI*YI);
}

double ModSquare (Cplfloat Z){
	return(Reprodconj((double)Z.re, (double)Z.im,(double) Z.re,(double)Z.im));
}



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



void RecordPlots( char* name, int NbPlots, Cplfloat Plots[NbPlots], float rg_size, 
  int Nv,float Vmax,int Nth, float Beamwidth){

    FILE *fp;
    int pl, rg, v, th;
    float Distance, Velocity, Angle;
    fp= fopen(name, "w");
    if(fp){
	    for (pl=0; pl<NbPlots; pl++) {
	        Cplfloat PLOT = Plots[pl];
	        if (PLOT.im!=0.) {
	            rg = (int)(PLOT.re/10000. +.1);
	            Distance = rg_size * rg;
	            v = (int)( (PLOT.re - 10000.*rg)/100. +.1);
	            Velocity = vint2Vfloat_1 (v, Nv,Vmax); 
	            th = (int)(PLOT.re - 10000.*rg -100.*v +.1);
	            Angle = thint2THfloat(th, Nth, Beamwidth);
	            Angle = 90. - Angle*(180./Pi);
	            fprintf(fp, " %2d:  Distance  %e  (%2d)     Velocity %e  ( %2d)     Angle %e ( %2d) _  Peak  %e\n", pl, Distance,rg,Velocity, v,Angle, th, PLOT.im);
	        }
	    }
	    fclose(fp);
	}  	   
}	



/***********************************************************************************************/
// Read inputs from file InputStimuli.txt 

int HasNextBurst(FILE *fp, float *Tr ){
     int status =0;	
	char line[256];
	 char *variable;
	int chara;
	 int i;
	
	 //First significant line should contain pulse repetition period, "Tr=",
	 // and the float value of Tr
	
	 int HasEqual=0; 
	 int EndOfFile =0;
	 while( (!HasEqual)&&(!EndOfFile)){ 
		 HasEqual= 0;
		 	 i=0;
		 while(i<255) {
		   chara=fgetc(fp);
		   (line[i]) = chara;
		   if(chara<0){
		   		EndOfFile=1;
		   		break;
		   }
		   if(line[i]=='=')
		   		HasEqual=1;
		   if(line[i]=='\n') break;
		   if(line[i]!=' ') i++;
		 }
		 line[i+1]='\0';
	 }
	 if(HasEqual){
	 	 status=1;
		 variable = strtok(line,"=");
 		 sscanf(strtok(NULL,"\n"), "%f", Tr);
	 }
     return status;
}


int GetNextBurst(FILE *fp, int Nrg, int Nsa, int Npul, Cplfloat Tab[Npul][Nsa][Nrg]) {


  float X;
    int pul, sa,rg;


		for(pul=0; pul<Npul;pul++) {
			for(sa=0; sa<Nsa;sa++) {
				for(rg=0; rg<Nrg;rg++) {
		            fscanf(fp, "%f", &X);
		            Tab[pul][sa][rg].re = X;
		            fscanf(fp, "%f", &X);
		            Tab[pul][sa][rg].im = X;
		    }
			}
		}

 return 0;
}

/***********************************************************************************************/


void init_tab_coh(int nv, int dim, float Tr, Cplfloat tab_coh[nv][dim]) {
    int iv, idop;
    float V, time;
    float Vmax= ATL_Lambda/(4.* Tr);

    for (iv=0; iv<nv; iv++) {
        V= Vmax * (2.*iv+1.)/(1.*nv);
        if (iv>=nv/2) {
            V=V-2*Vmax;
        }
        for (idop=0; idop<dim; idop++) {
            time = 2 * Pi * idop * 2 * V * Tr * (1./ATL_Lambda);
            tab_coh[iv][idop].re= cos(time);
            tab_coh[iv][idop].im= -sin(time);
        }
    }
}

/***********************************************************************************************/

void init_chirp(int tf, Cplfloat Chirp[tf]) {
 int i=0;
        Chirp[i].re=1.;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.93461;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.832174452;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.617829847;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.309016994;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=-0.382683432;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=-0.891615484;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=-0.66535522;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.251076308;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.747688;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=-1.75436E-11;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=-0.623073334;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.173822059;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.380202983;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=-0.416087226;
        Chirp[i].im=0.;
        i++;
        Chirp[i].re=0.143506287;
        Chirp[i].im=0.;
        i++;
    
}

//  -----------------  stap_functions.c -----------------------------------------

/***********************************************************************************************/
void Calc_steervect(int nth, int nv, int ntt, int nsa,
		float SubarraySpacing,
		float Lambda,
		float BeamAngle,
		float CarrierSpeed,
		float BeamWidth,
		float TargetDistance,
		float Tr,
        Cplfloat Steervect[nth][nv][ntt*nsa]) {

		
    int ant, rec, v, th;
    float V;
    double AngleCarrier_rad= BeamAngle*Pi/180.;

    double theta;
    double Kdeph = 2.*Pi/Lambda;

    double angldir, delta_dist, deph, deph_a, deph_v;
    for (th=0; th<nth; th++) {
        theta = thint2THfloat(th, nth, BeamWidth);

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
            exit(0);
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
void average_power(int Nth, int Nrg, int Nv, Cplfloat ptrin[Nth][Nrg][Nv],
        Cplfloat Pow[Nth]) {

    double PP;
    int th, v, rg;

    for (th=0; th<Nth; th++) {
        PP=0.;
        for (rg=0; rg<Nrg; rg++) {
            for (v=0; v<Nv; v++) {
                PP += ptrin[th][rg][v].re *ptrin[th][rg][v].re
                        +ptrin[th][rg][v].im *ptrin[th][rg][v].im;
            }
        }
        Pow[th].re= (float)(PP/((float)(Nv*Nrg)));
        Pow[th].im= 0.;
    }
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


void Mat2Plot(int Nth, int Nv, int Nrg, int nbplots,
        Cplfloat In[Nrg][Nth][Nv], Cplfloat Out[nbplots]) {

    int th, v, rg;
    int pl=0;
    int i;
    double pow;

    for (rg=0; rg<Nrg; rg++) {
        for (v=0; v<Nv; v++) {
            for (th=0; th<Nth; th++) {
                pow= ModSquare(In[rg][th][v]);
                if ( (pow >1.E-5)&&(pl<nbplots)) {
                    Out[pl].re = (float)( 10000*rg+100*v+ th);
                    Out[pl].im = (float) pow;
                    pl++;
                }
            }
        }
    }

    for (i=pl; i<nbplots; i++) {
        Out[i].re = 0.;
        Out[i].im = 0.;
    }
} 


/***********************************************************************************************/
void RemAmbiguity(int nbplots, int Nraf, Cplfloat In[Nraf][nbplots], float pTr[Nraf],
        int Nrg, int Nv, int Nth, float rg_size, float lambda, Cplfloat Out[4*nbplots]) {

    
    int pl, raf,i;
    int th, v, newv0, rg;

    memset(Out, 0, 4*nbplots*sizeof(Cplfloat));

    float TAB_RGVTH[Nrg][2*Nv][Nth];
    int TAB_NBHIT[Nrg][2*Nv][Nth];

    for (rg=0; rg<Nrg; rg++) {
        for (v=0; v<2*Nv; v++) {
            for (th=0; th<Nth; th++) {
                TAB_RGVTH[rg][v][th]=0.;
                TAB_NBHIT[rg][v][th]=0;
            }
        }
    }


    float V;
    float Trmin= pTr[0];
    for(raf=0; raf<Nraf;raf++){
    	if(pTr[raf]<Trmin){
    		Trmin=pTr[raf];
    	}
    }
    float Vmax0 = lambda/(4.*Trmin);
    	
    
    Cplfloat PLOT, NEW_PLOT; 
    for (raf=0; raf<Nraf; raf++) {
        float Trec = pTr[raf];
        for (pl=0; pl<nbplots; pl++) {
            PLOT = In[raf][pl];
            if (PLOT.im!=0.) {
                rg = (int)(PLOT.re/10000. +.1);
                v = (int)( (PLOT.re - 10000.*rg)/100. +.1);
                th = (int)(PLOT.re - 10000.*rg -100.*v +.1);               
                V = vint2Vfloat(v, Nv, lambda, Trec);
                float Vmax= lambda/(4*Trec);
                for(i=-2;i<3;i++){
                	if( (V + 2*i*Vmax>-2*Vmax0) && (V + 2*i*Vmax<2*Vmax0)){
                 		newv0 = Vfloat2vint(V + 2*i*Vmax, 2*Nv, lambda, Trmin/2.);
   						TAB_RGVTH[rg][newv0][th] += PLOT.im;
                		TAB_NBHIT[rg][newv0][th] += 1;
                	}
                }
           }
        }
    }
    
    int RG_WINDOW=1;
    int V_WINDOW=1;
    int TH_WINDOW=1;
    int a, b, c;
    pl=0;
 
    for (rg=RG_WINDOW; rg<Nrg-RG_WINDOW; rg++) {
        for (v=V_WINDOW; v<2*Nv-V_WINDOW; v++) {
            for (th=TH_WINDOW; th<Nth-TH_WINDOW; th++) {
            	if(TAB_NBHIT[rg][v][th]>0){
	                int isMax=1;
	                int nb_hits=TAB_NBHIT[rg][v][th];
	                float pow = TAB_RGVTH[rg][v][th];
	                float add_pow=0.;
	                for (a=-RG_WINDOW; a<=RG_WINDOW; a++) {
	                    for (b=-V_WINDOW; b<=V_WINDOW; b++) {
	                        for (c=-TH_WINDOW; c<=TH_WINDOW; c++) {
	                        	float pow_a= TAB_RGVTH[rg+a][v+b][th+c];
	                        	int h = TAB_NBHIT[rg+a][v+b][th+c];
	                            if (((a!=0)||(b!=0)||(c!=0))&&(h!=0)){
	                                nb_hits+=h;
	                                add_pow+= pow_a;
	                                if (pow_a >pow) {
	                                    isMax=0;
	                                }
	                            }
	                        }
	                    }
 	                }
	                if ( (isMax>0) &&(nb_hits>= .8*Nraf)) {
	                    NEW_PLOT.re = 10000.*rg + 100.*v + 1.*th;
	                    NEW_PLOT.im = pow + add_pow;
	                    Out[pl] = NEW_PLOT;
	                    pl++;
	                }
            	}
            }
        }
    }
 
    while (pl<2*nbplots) {
        NEW_PLOT.re = 0.;
        NEW_PLOT.im = 0.;
        Out[pl] = NEW_PLOT;
        pl++;
    }
    

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
void turn5(int dim1, int ntt, int nsa, Cplfloat a[dim1][ntt][nsa][ntt][nsa],
        Cplfloat b[nsa][ntt][nsa][ntt][dim1]) {
    int i, j, k, l, t;

    for (t=0; t<dim1; t++) {
        for (i=0; i<ntt; i++) {
            for (j=0; j<nsa; j++) {
                for (k=0; k<ntt; k++) {
                    for (l=0; l<nsa; l++) {
                        b[i][j][k][l][t].re = a[t][i][j][k][l].re;
                        b[i][j][k][l][t].im = a[t][i][j][k][l].im;
                    }
                }
            }
        }
    }
}

/***********************************************************************************************/
void X_3(int dim1, int dim2, int dim3, Cplfloat a[dim1][dim2][dim3],
        Cplfloat b[dim2][dim1][dim3]) {
    int i, j, k;

    for (i=0; i<dim1; i++) {
        for (j=0; j<dim2; j++) {
            for (k=0; k<dim3; k++) {
                b[j][i][k].re = a[i][j][k].re;
                b[j][i][k].im = a[i][j][k].im;

            }
        }
    }
}



/***********************************************************************************************/
void X_4(int dim1, int dim2, int dim3, Cplfloat a[dim1][dim2][dim3],
        Cplfloat b[dim2][dim3][dim1]) {
    int i, j, k;

    for (i=0; i<dim1; i++) {
        for (j=0; j<dim2; j++) {
            for (k=0; k<dim3; k++) {
                b[j][k][i].re = a[i][j][k].re;
                b[j][k][i].im = a[i][j][k].im;
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
/***    Bursts processing  ******************************************************************/
/***********************************************************************************************/


void trt_burst(int nrg, int tf, int nv, int nth, int nsa, int ntt, int nrec, int nplots, int Tfac_SizeV, int Tfac_sizeRG, int Tfac_K, int Tfac_WK,
        float SubarraySpacing, float Lambda, 
		float BeamAngle,float CarrierSpeed,float BeamWidth, float TargetDistance,float Tr,
		Cplfloat in_pulse[nsa][nrec][nrg],
        Cplfloat out_plot[nplots] ) {

    int nrgf = nrg-tf+1;
    int NrgLarge=  nrgf + Tfac_sizeRG -1; 
    int NvLarge= nv + Tfac_SizeV -1;
    int j, k, l;



    /* Parameters for kernels */
    static int data[8] __attribute__((aligned(128)));
    static float fdata[8] __attribute__((aligned(128)));

    int i; 


//     init_steervect(nth, nv, ntt, nsa, Steervect);


#if 0
    data[0] = nv;
    data[1] = ntt;
    data[2] = nsa;

    fdata[0] = SubarraySpacing;
    fdata[1] = Lambda;
    fdata[2] = BeamAngle; 
    fdata[3] = CarrierSpeed;  
    fdata[4] = BeamWidth;  
    fdata[5] = TargetDistance;  
    fdata[6] = Tr;

    for ( i=0; i<nth; i++){

        tpc_call( 15, 3,
                        Steervect[i] , nv*ntt*nsa*sizeof(Cplfloat) , TPC_INOUT_ARG,
                        fdata, sizeof(fdata), TPC_IN_ARG,
                        data, sizeof(data), TPC_IN_ARG
                );
    }
    tpc_wait_all();
#else

    Calc_steervect(nth, nv, ntt, nsa,SubarraySpacing, Lambda, 
		BeamAngle,CarrierSpeed,BeamWidth, TargetDistance,Tr, Steervect); 	
#endif


    init_chirp(tf, filtre);




    for (j=0;j<MAX_SPES;j++)
        tpc_call( 1, 1,
                filtre, sizeof(filtre),TPC_IN_ARG
         );

    init_tab_coh(nv, nrec-ntt+1, Tr,tab_coh);


    data[0] = nrg;
    data[1] = tf;
    data[2] = nrec;

    for (j=0; j<nsa; j++) {
        for (k=0; k<nrec; k++) {

            tpc_call( 2, 3,
                    out_pulse[j][k], (nrg-tf+1)*sizeof(Cplfloat), TPC_OUT_ARG,
                    in_pulse[j][k], (nrg)*sizeof(Cplfloat), TPC_IN_ARG,
                    data, sizeof(data), TPC_IN_ARG
                    );


        }

    }
    tpc_wait_all();



    X_2(nsa, nrec, nrgf, out_pulse, in_cov);




    
    /* X_1  can be executed parallel */
    data[0] = 1;
    data[1] = nsa;
    data[2] = nrec;
    data[3] = ntt;

    for (i=0; i< nrgf;i++){
        tpc_call( 3, 3,
                &in_apply[i][0][0] , (nsa*nrec)*sizeof(Cplfloat), TPC_OUT_ARG,
                &in_cov[i][0][0]   , (nsa*nrec)*sizeof(Cplfloat), TPC_IN_ARG,
                data, sizeof(data), TPC_IN_ARG
                );
        tpc_call( 5, 3,
                out_avcov[i], (nsa*ntt)*(nsa*ntt)*sizeof(Cplfloat), TPC_OUT_ARG,
                in_cov[i],(nsa*nrec)*sizeof(Cplfloat), TPC_IN_ARG,
                data, sizeof(data), TPC_IN_ARG
                );
    }

    tpc_wait_all();


    data[0] = ntt;
    data[1] = nsa;
    for (j=0; j<nrgf; j++) {
        /* Arguments of this tpc call are big enough: no prefetching */
        tpc_call( 6, 3,
                mat_inv[j], (nsa*ntt)*(nsa*ntt)*sizeof(Cplfloat), TPC_OUT_ARG,
                out_avcov[j],  (nsa*ntt)*(nsa*ntt)*sizeof(Cplfloat), TPC_IN_ARG,
                data, sizeof(data), TPC_IN_ARG
                );

    }
    tpc_wait_all();

    data[0] = ntt;
    data[1] = nsa;
    data[2] = nv;

    for (j=0; j<nrgf; j++) {
    	 for (k=0; k<nth; k++) {   	
        tpc_call( 7, 4,
                Fil[j][k],    (nv)*(nsa*ntt)*sizeof(Cplfloat), TPC_OUT_ARG,
                mat_inv[j],  (nsa*ntt)*(nsa*ntt)*sizeof(Cplfloat), TPC_IN_ARG,
                Steervect[k],  (nv)*(nsa*ntt)*sizeof(Cplfloat), TPC_IN_ARG,
                data, sizeof(data), TPC_IN_ARG
                );
    	 }
    }

    tpc_wait_all(); 





    data[0] = nrec;
    data[1] = ntt;
    data[2] = nsa;

    for (j=0; j<nrgf; j++) {
        for (k=0; k<nth; k++) {
            for (l=0; l<nv; l++) {

                tpc_call( 8, 4,
                        out_apply[j][k][l],    (nrec-ntt+1)*sizeof(Cplfloat), TPC_OUT_ARG,
                        in_apply[j],  (nrec)*(nsa)*sizeof(Cplfloat), TPC_IN_ARG,
                        Fil[j][k][l],  (ntt*nsa)*(nsa*ntt)*sizeof(Cplfloat), TPC_IN_ARG,
                        data, sizeof(data), TPC_IN_ARG
                        );
            }
        }
    }

    tpc_wait_all();



    data[0] = nv;
    data[1] = nrec-ntt+1;

    for (j=0; j<nrgf; j++) {
        for (k=0; k<nth; k++) {
                tpc_call( 9, 4,
                        out_int_dop[j][k],    (nv)*sizeof(Cplfloat), TPC_OUT_ARG,
                        out_apply[j][k],  (data[1])*(nv)*sizeof(Cplfloat), TPC_IN_ARG,
                        tab_coh,  (data[1])*(nv)*sizeof(Cplfloat), TPC_IN_ARG,
                        data, sizeof(data), TPC_IN_ARG
                        );
        }
    }
 
    tpc_wait_all();


    X_3(nrgf, nth, nv, out_int_dop, in_avpow);

    average_power(nth, nrgf, nv, in_avpow, out_avpow);

    /* tfac add edges */
    data[0] = nrgf;
    data[1] = nv;
    data[2] = NrgLarge;
    data[3] = NvLarge;
    for (i=0; i<nth;i++){

             tpc_call( 14, 4,
                        out_facedges[i],  (NvLarge * NrgLarge) *sizeof(Cplfloat), TPC_INOUT_ARG,
                        in_avpow[i],  ( nrgf*nv )*sizeof(Cplfloat), TPC_IN_ARG,
                        &out_avpow[i],  sizeof(Cplfloat), TPC_IN_ARG,
                        data, sizeof(data), TPC_IN_ARG
                        );

    }
   tpc_wait_all();




    /* tfac */
    data[0] = NrgLarge;
    data[1] = NvLarge;
    data[2] = Tfac_SizeV;
    data[3] = Tfac_sizeRG;
    fdata[0] = Tfac_K;
    fdata[1] = Tfac_WK;
    for (i=0; i<nth; i++){
                /* Args here are not so good...: indexes out of range  */
                tpc_call( 13, 4,
                        out_face1[i],  (  NvLarge - Tfac_sizeRG  +1) * ( NrgLarge - Tfac_SizeV  +1)  *sizeof(Cplfloat), TPC_INOUT_ARG,
                        out_facedges[i],  (NrgLarge*NvLarge )*sizeof(Cplfloat), TPC_IN_ARG,
                        data, sizeof(data), TPC_IN_ARG,
                        fdata, sizeof(fdata), TPC_IN_ARG
                        );
    }
    tpc_wait_all();



    X_4(nth, nrgf, nv, out_face1, in_maxpow);


    data[0]=nth;
    for (j=0; j<nrgf; j++) {
        for (k=0; k<nv; k++) {

        tpc_call( 10, 3,
                out_maxpow[j][k], (nth)*sizeof(Cplfloat), TPC_OUT_ARG,
                in_maxpow[j][k],  (nth)*sizeof(Cplfloat), TPC_IN_ARG,
                data, sizeof(data), TPC_IN_ARG
                );
        }
    }

    tpc_wait_all();


    data[0] = nth;
    data[1] = nv;
    for (j=0; j<nrgf; j++) {

        tpc_call( 11, 3,
                in_correctionV[j], (nth*nv)*sizeof(Cplfloat), TPC_OUT_ARG,
                out_maxpow[j],  (nth*nv)*sizeof(Cplfloat), TPC_IN_ARG,
                data, sizeof(data), TPC_IN_ARG
                );
    }
    tpc_wait_all();


    data[0] = nth;
    data[1] = nv;
    fdata[0] = BeamWidth;
    fdata[1] = BeamAngle;
    fdata[2] = Tr;
    fdata[3] = Lambda;
    fdata[4] = CarrierSpeed;

    for (j=0; j<nrgf; j++) {
                tpc_call( 12, 4,
                        out_correctionV[j],    nth*(nv)*sizeof(Cplfloat), TPC_OUT_ARG,
                        in_correctionV[j],  nth*nv*sizeof(Cplfloat), TPC_IN_ARG,
                        data, sizeof(data), TPC_IN_ARG,
                        fdata, sizeof(fdata), TPC_IN_ARG
                        );


    }
    tpc_wait_all();



    Mat2Plot(nth, nv, nrgf, nplots, out_correctionV, out_plot);

}

static Cplfloat in_pulse[NRAF_MAX][ATL_nsa][ATL_nrec][ATL_Nrg]   ;

/***********************************************************************************************/
/*** MAIN *************************************************************************************/
/***********************************************************************************************/
int main(int argc, char** argv) {

    int Nraf=0;
    int i=0;
    unsigned int c;
    unsigned int nprocs=0;

    static Cplfloat out_plot[NRAF_MAX][ATL_Nplots];
    static Cplfloat amb_out[ATL_Nplots*4];

    float Tr[NRAF_MAX];
    FILE *fp;

    char *filename="InputStimuli.txt";


    while ((c = getopt(argc, argv, "p:h")) != -1) {
    	switch(c) {
    	case 'p':
    		nprocs = atoi(optarg);
			if (nprocs < 0 || nprocs > 16) {
				perror("P must be between {0-16} SPE's. 0 means serial execution.\n");
				exit(-1);
			}
			break;
    	case 'h':
    		printf("Usage: %s <options>\n\n",argv[0]);
    		printf("  -pP : P = number of processors. Must be {0-16} SPE's. Default = 0\n");
    		printf("  -h  : Print help screen.\n\n");
                exit(0);
    		break;
    	}

    }
    


    //Open input file
    if( (fp=fopen(filename,"rb"))==NULL) {
        fprintf(stderr,"couldn't open \"%s\"!\n",filename);
        return 1;
    }

    tpc_init(nprocs);

    float Trn=0.;
    while( HasNextBurst(fp, &Trn)){		
        GetNextBurst(fp, ATL_Nrg, ATL_nsa, ATL_nrec, in_pulse[Nraf]) ;
        Tr[Nraf]=Trn;
        Nraf++;
    }
    tpc_reset_stats();
    uint64_t tmp_time0 =  __mftb();
    for (i=0; i<Nraf; i++) {
        trt_burst(ATL_Nrg, ATL_tf, ATL_Nv, ATL_Nth, ATL_nsa,ATL_ntt, ATL_nrec, ATL_Nplots, 
                ATL_tfac_L_V, ATL_tfac_L_RG, ATL_tfac_K , ATL_tfac_AVPWK, 
                ATL_SubarraySpacing, ATL_Lambda, 
                ATL_BeamAngle,ATL_CarrierSpeed,ATL_BeamWidth, ATL_TargetDistance,/*ATL_Tr*/ Tr[i],
                in_pulse[i], out_plot[i] );
    }
    RemAmbiguity(ATL_Nplots, Nraf, out_plot, Tr, ATL_Nrg-ATL_tf+1, ATL_Nv,ATL_Nth, ATL_rg_size, ATL_Lambda, amb_out);


    uint64_t tmp_time1 =  __mftb();
    tpc_print_stats(stdout);
    float Trmin= Tr[0];
    for(i=0; i<Nraf; i++){
        if(Tr[i]<Trmin){
            Trmin=Tr[i];
        }
    }
    printf("Total CPU cell ticks: %llu\n",tmp_time1-tmp_time0);

    RecordPlots( "PlotsOut.txt", ATL_Nplots*4,amb_out, ATL_rg_size, 2*ATL_Nv,ATL_Lambda/(2.*Trmin),ATL_Nth,ATL_BeamWidth  );

    printf("STAP - end of program.\n# file 'PlotsOut.txt' generated.\n");

    return(0);

} 
