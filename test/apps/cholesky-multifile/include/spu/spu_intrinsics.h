
/* Definitions of Synergistic Processing Unit (SPU). */

/* (C) Copyright
   Sony Computer Entertainment, Inc.,
   2001,2002,2003,2004,2005,2006.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* As a special exception, if you include this header file into source files 
   compiled by GCC, this header file does not by itself cause  the resulting 
   executable to be covered by the GNU General Public License.  This exception 
   does not however invalidate any other reasons why the executable file might be 
   covered by the GNU General Public License.  */ 

#ifndef  _SPU_INTRINSICS_H
#define _SPU_INTRINSICS_H 
 
#define vec_uchar16             vector unsigned char
#define vec_char16              vector   signed char
#define vec_ushort8             vector unsigned short
#define vec_short8              vector   signed short
#define vec_uint4               vector unsigned int
#define vec_int4                vector   signed int
#define vec_ullong2             vector unsigned long long
#define vec_llong2              vector   signed long long
#define vec_float4              vector          float
#define vec_double2             vector          double

/* SPU Channel Defines 
 */
#define SPU_RdEventStat		 0
#define SPU_WrEventMask		 1
#define SPU_WrEventAck		 2
#define SPU_RdSigNotify1	 3
#define SPU_RdSigNotify2	 4
#define SPU_WrDec		 7
#define SPU_RdDec		 8
#define SPU_RdEventMask		11
#define SPU_RdMachStat		13
#define SPU_WrSRR0		14
#define SPU_RdSRR0		15
#define SPU_WrOutMbox		28 
#define SPU_RdInMbox		29 
#define SPU_WrOutIntrMbox	30 

/* MFC Channel Defines. 
 */
#define MFC_WrMSSyncReq		 9
#define MFC_RdTagMask		12
#define MFC_LSA			16 
#define MFC_EAH			17 
#define MFC_EAL			18 
#define MFC_Size		19 
#define MFC_TagID		20 
#define MFC_Cmd			21 
#define MFC_WrTagMask		22 
#define MFC_WrTagUpdate		23 
#define MFC_RdTagStat		24 
#define MFC_RdListStallStat	25 
#define MFC_WrListStallAck	26 
#define MFC_RdAtomicStat	27 

/* Bit flag mnemonics for test special value.
 */
#define SPU_SV_NEG_DENORM	0x01	/* Test for a negative denormalized number.  */
#define SPU_SV_POS_DENORM	0x02	/* Test for a positive denormalized number.  */
#define SPU_SV_NEG_ZERO		0x04	/* Test for a negative zero.  */
#define SPU_SV_POS_ZERO		0x08	/* Test for a positive zero.  */
#define SPU_SV_NEG_INFINITY	0x10	/* Test for a negative infinity.  */
#define SPU_SV_POS_INFINITY	0x20	/* Test for a positive infinity.  */
#define SPU_SV_NAN		0x40	/* Test for a not a number, both signaling and quite.  */

#include <spu_internals.h>

#endif /* _SPU_INTRINSICS_H */
