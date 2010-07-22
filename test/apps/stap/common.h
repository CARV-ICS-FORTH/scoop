#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/**********************************
 * System parameters
 ********************************* */

#define Pi 3.141592653589793238
#define ATL_Lambda 0.03
#define ATL_Tr 0.0006  // 5.0E-4

#define ATL_Nrg 95			// number of range gates
#define ATL_tf 16			// filter length (16-tap)
#define ATL_Nrgf   ATL_Nrg-ATL_tf+1	
#define ATL_ntt 5			// number of pulses in sub-window
#define ATL_nsa 5			// number of antennas (=Nant)	
#define ATL_nrec 32			// number of pulses in a burst
#define ATL_tfac_L_V 5
#define ATL_tfac_L_RG 7
#define ATL_tfac_K   10.0
#define ATL_tfac_AVPWK  10000.0

#define ATL_Nth 20			// number of "Theta" points assumptions
#define ATL_Nv 20			// number of "velocity" points assumptions

#define ATL_Nrg_enlarged  ATL_Nrgf +  ATL_tfac_L_RG -1   // 86
#define ATL_Nv_enlarged ATL_Nv +  ATL_tfac_L_V -1   // 24

#define ATL_CarrierSpeed 200
#define ATL_BeamAngle 90
#define ATL_BeamWidth 5
#define ATL_SubarraySpacing 0.2
#define ATL_TargetDistance 12000
#define ATL_Nplots 100
#define ATL_rg_size  15

#define NRAF_MAX 10


/***********************************************************************************************/
// Define complex number
typedef struct {
    float re;
    float im;
} __attribute__((aligned(16))) Cplfloat;
/***********************************************************************************************/




