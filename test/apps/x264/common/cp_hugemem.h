// cp_hugemem.h 
//
// Copyright (c) 2006, Mike Acton <macton@cellperformance.com>
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
// documentation files (the "Software"), to deal in the Software without restriction, including without
// limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial
// portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
// LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
// EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
// AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef CP_HUGEMEM_H
#define CP_HUGEMEM_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <stdint.h>

typedef struct cp_hugemem cp_hugemem;
struct cp_hugemem
{
    int        fd;
    void *   addr;
    size_t     length;
};

int   cp_find_hugetlbfs_mount( char* mount_path );
int   cp_hugemem_alloc( cp_hugemem* hugemem, size_t size );
void  cp_hugemem_free( cp_hugemem* hugemem );

#if defined(__cplusplus)
}
#endif
#define PATH_HMAX 64
extern char  huge_file_name[PATH_HMAX+1] ;
extern cp_hugemem hmem;

#endif
