/* Some common inline functions for the different CELL conv2d::leaf implementations.
 */

#ifndef __CONV2D_LEAF_CELL_H__
#define __CONV2D_LEAF_CELL_H__

static __inline vector float load_misaligned_vector_float__(int shift, vector float v0123, vector float v4567) {
    vector float qw0, qw1;
    qw0 = v0123;
    qw1 = v4567;
    return spu_or(spu_slqwbyte(qw0, shift), spu_rlmaskqwbyte(qw1, shift-16));
}

static __inline float sum_across_float4__(vector float v)
{
    vector float c12, c2, c3, c4, c34;
    vector float result;
    c2 = spu_rlqwbyte(v, 4);
    c3 = spu_rlqwbyte(v, 8);
    c4 = spu_rlqwbyte(v, 12);
    c12 = spu_add(v,  c2);
    c34 = spu_add(c3, c4);
    result = spu_add(c12, c34);
    return (spu_extract(result, 0));
}

// Return a vector in which element i is the sum of all elements in the argument vector v<i>.
static __inline vector float sum_across_four_float4s(vector float v0, 
                                                     vector float v1, 
                                                     vector float v2, 
                                                     vector float v3,
                                                     vector unsigned int mask000f,
                                                     vector unsigned int mask00f0,
                                                     vector unsigned int mask0f00,
                                                     vector unsigned int maskf000) {

    // Want to make 4 vectors, one with the first elements of v0,v1,v2,v3, one with the second elements, and so on.
    // Do this by first rotating v1,v2,v3 so that none of the elements are in the same "columns" (i.e.) so that
    // the first element of v0 is in pos0, the first element of v1 is in pos1, etc.
    // Then construct the vectors containing the first, second, etc. elements of the 4 argument vectors. 
    // Then, perform an addition across the 4 constructed vectors, and return the result.
    
    vector float v1r1 = spu_rlqwbyte(v1, 12); // Same as rotating by -4
    vector float v2r2 = spu_rlqwbyte(v2, 8); // Same as rotating by -8
    vector float v3r3 = spu_rlqwbyte(v3, 4); // Same as rotating by -12

    // v0 = ABCD
    // v1 = EFGH
    // v2 = IJKL
    // v3 = MNOP
    //
    // v0   = ABCD
    // v1r1 = HEFG
    // v2r2 = KLIJ
    // v3r3 = NOPM
    //
    // velmt0s = AEIM
    // velmt1s = BFJN
    // velmt2s = CGKO
    // velmt3s = DHLP

    vector float velmt0s = spu_sel(v0, v1r1, mask0f00);
    velmt0s = spu_sel(velmt0s, v2r2, mask00f0);
    velmt0s = spu_sel(velmt0s, v3r3, mask000f);
    
    vector float velmt1s = spu_sel(v0, v1r1, mask00f0);
    velmt1s = spu_sel(velmt1s, v2r2, mask000f);
    velmt1s = spu_sel(velmt1s, v3r3, maskf000);
    
    vector float velmt2s = spu_sel(v0, v1r1, mask000f);
    velmt2s = spu_sel(velmt2s, v2r2, maskf000);
    velmt2s = spu_sel(velmt2s, v3r3, mask0f00);
    
    vector float velmt3s = spu_sel(v0, v1r1, maskf000);
    velmt3s = spu_sel(velmt3s, v2r2, mask0f00);
    velmt3s = spu_sel(velmt3s, v3r3, mask00f0);

    vector float vsums01 = spu_add(velmt0s, velmt1s);
    vector float vsums23 = spu_add(velmt2s, velmt3s);
    vector float vsums = spu_add(vsums01, vsums23);
    return vsums;
}

#endif

