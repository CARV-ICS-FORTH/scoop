void TwiddleOneCol_vec(direction, n1, N, u, x, pad_length)
int direction; 
int n1;
int N;
double *u;
double *x;
int pad_length;
{
  int i;
  vector double omega, omega_b, omega_dir={1.0, direction};
  vector double tmpx;
  vector double rrcc, rcrc;
  vector double rrcc_b, rcrc_b;
  vector double rrcc_c, rcrc_c;
  vector double *vecu = (vector double *)u;
  vector double *vecx = (vector double *)x;
  
  vector unsigned char vpat1 = (vector unsigned char) { 0x00, 0x01, 0x02, 0x03,
                                                        0x04, 0x05, 0x06, 0x07,
                                                        0x18, 0x19, 0x1A, 0x1B,
                                                        0x1C, 0x1D, 0x1E, 0x1F };

  for (i=0; i<n1; i++) {
    omega = vecu[i];
    omega *= omega_dir;
    omega_b = spu_rlqwbyte(omega, 8); // invert omega
    tmpx = vecx[i];
    
    rrcc = omega * tmpx;
    rcrc = omega_b * tmpx;
    
    rrcc_b = spu_rlqwbyte(rrcc, 8);
    rcrc_b = spu_rlqwbyte(rcrc, 8);

    rrcc_c = rrcc - rrcc_b;
    rcrc_c = rcrc + rcrc_b;

    vecx[i] = spu_shuffle(rrcc_c, rcrc_c, vpat1);

  }
}



void TwiddleOneCol_vec2(direction, n1, N, u, x, pad_length)
int direction; 
int n1;
int N;
double *u;
double *x;
int pad_length;
{
  int i;
  register vector double omega;
  register vector double omega_b;
  register vector double tmpx;
  register vector double cc;
  register vector double cr;
  vector double *vx = (vector double *)x;
  vector double *vu = (vector double *)u;
  register vector double vdir = {1.0, (double)direction};
  vector unsigned char vpat1 = (vector unsigned char) { 0x00, 0x01, 0x02, 0x03,
                                                        0x04, 0x05, 0x06, 0x07,
                                                        0x18, 0x19, 0x1A, 0x1B,
                                                        0x1C, 0x1D, 0x1E, 0x1F };

  for (i=0; i<n1; i++) {
    omega = vu[i]*vdir;
    tmpx = vx[i];
    omega_b = spu_rlqwbyte(omega, 8); // invert omega

    cc = omega * tmpx;
    cr = omega_b * tmpx;

    cc = spu_rlqwbyte(cc, 8); // invert
    cr = spu_rlqwbyte(cr, 8); // invert

    cc = spu_msub(omega, tmpx, cc);
    cr = spu_madd(omega_b, tmpx, cr);
        
    vx[i] = spu_shuffle(cc, cr, vpat1);
  
  }
}

