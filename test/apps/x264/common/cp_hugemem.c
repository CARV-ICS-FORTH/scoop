// cp_hugemem.c 
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

#ifdef TPC
#ifdef HPAGE

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <mntent.h>
//#include <asm/page.h>
#include <errno.h>
#include <error.h>
#include "cp_hugemem.h"

#define LARGE_PAGES_PATH /huge

char  huge_file_name[PATH_HMAX+1] ="LARGE_PAGES_PATH" ;
cp_hugemem    hmem;


// Find the directory where tlbfs is mounted.
// NOTE: mount_path size must be at least (PATH_MAX+1).
int
cp_find_hugetlbfs_mount( char* mount_path )
{
    FILE*           mount_table;
    struct mntent*  mount_entry;

    // From Linux Programmer's Manual (man setmntent):
    // 
    //   The  setmntent() function opens the file system description file fp and returns a file pointer which can be used by getmntent().  The argument type is the type of
    //   access required and can take the same values as the mode argument of fopen(3).
    //
    //   The getmntent() function reads the next line from the file system description file fp and returns a pointer to a structure containing the broken out fields from a
    //   line in the file.  The pointer points to a static area of memory which is overwritten by subsequent calls to getmntent().
    //
    //   The endmntent() function closes the file system description file fp.
    //
    //   The mntent structure is defined in <mntent.h> as follows:
    //
    //          struct mntent {
    //                  char    *mnt_fsname;    /* name of mounted file system */
    //                  char    *mnt_dir;       /* file system path prefix */
    //                  char    *mnt_type;      /* mount type (see mntent.h) */
    //                  char    *mnt_opts;      /* mount options (see mntent.h) */
    //                  int     mnt_freq;       /* dump frequency in days */
    //                  int     mnt_passno;     /* pass number on parallel fsck */
    //          };
    //

    int  mount_table_retry_max   = 128;
    int  mount_table_retry_count = 0;

    while ( mount_table_retry_count < mount_table_retry_max )
    {
        mount_table = setmntent(_PATH_MOUNTED, "r");

        if (!mount_table) 
        {
           if ( ( errno == EACCES )  // File is locked. Try again
             || ( errno == EAGAIN )   // File is probably in the process of being locked. Try again.
             || ( errno == ENFILE ) ) // No available handles in the system. Try again.
           {
               sleep(1);
               mount_table_retry_count++;
               continue;
           } 
          
           // Anything else is catastrophic.
           fprintf(stderr,"Error: Some major unexpected problem when trying to get a lock on the mount table.\n");
           return (0);
        }

        break;
    }

    if ( mount_table_retry_count == mount_table_retry_max )
    {
        fprintf(stderr,"Error: Couldn't get a lock on the mount table.\n");
        return (0);
    }

    mount_entry  = getmntent(mount_table);
    while (mount_entry) 
    {
        // Found the the right entry...
        if (strcmp(mount_entry->mnt_type, "hugetlbfs") == 0) 
        {
            strncpy(mount_path, mount_entry->mnt_dir, PATH_HMAX);
            endmntent(mount_table);
            return (1);
        }

        mount_entry = getmntent(mount_table);
    }

    endmntent(mount_table);

    fprintf(stderr,"###############################################################################################################\n");
    fprintf(stderr,"# Error: No hugetlbfs entry encounted in the mount table. Are you sure it's enabled? - Using simple memaling! #\n");
    fprintf(stderr,"###############################################################################################################\n");
    return (0);

}

// Allocate memory from huge pages in userspace.
int
cp_hugemem_alloc( cp_hugemem* hugemem, size_t size )
{
    //
    // Find the mount point for the huge tlb file system (hugetlbfs)
    //

    void* huge_addr;
//    char  huge_file_name[PATH_MAX+1];
    int   huge_fd;

    int  found_tlbfs_mount = cp_find_hugetlbfs_mount( huge_file_name );
    if (!found_tlbfs_mount)
    {
        return (0);
    }

    //
    // Create a file in the hugetlbfs to mmap
    //

    // From Linux Programmer's Manual (man mkstemp):
    //
    //   The  mkstemp()  function  generates  a unique temporary file name from name.  The last six characters of name must be XXXXXX and these are replaced with a
    //   string that makes the filename unique. The file is then created with mode read/write and permissions 0666 (glibc 2.0.6 and earlier), 0600 (glibc 2.0.7 and later).
    //   Since  it will be modified, name must not be a string constant, but should be declared as a character array.  The file is opened with the O_EXCL flag, guaran-
    //   teeing that when mkstemp() returns successfully we are the only user.

    const int   tmp_suffix_size = 7;
    const char  tmp_suffix[]    = "/XXXXXX";


    if ( strlen(huge_file_name) >= ( PATH_MAX-tmp_suffix_size ) )
    {
        fprintf(stderr,"Error: Mount point name length doesn't leave enough room for temporary file name.\n");
        return (0);
    }
    strcat(huge_file_name, tmp_suffix);


    huge_fd = mkstemp(huge_file_name);
    if (huge_fd == -1) 
    {
	perror("mkstemp");
        fprintf(stderr,"Error: Couldn't create file. Probably don't have permission set properly.\n");
        return (0);
    }

    // Don't need the file itself for mmap.
    unlink(huge_file_name);

    //
    // Map the huge page(s) to user memory
    //

    // From Linux Programmer's Manual (man mmap):
    //
    //   The mmap() function asks to map length bytes starting at offset offset from the file (or other object) specified by the file descriptor fd into memory, preferably
    //   at address start.  This latter address is a hint only, and is usually specified as 0.  The actual place where the object is mapped is returned by mmap().
    //size= 3* 16 * 1024 * 1024;
    huge_addr = mmap(0 , size, PROT_READ|PROT_WRITE, MAP_SHARED, huge_fd, 0);
    if (huge_addr == MAP_FAILED) 
    {
	perror("mmap");
	fprintf(stderr,">> fd:%d size:%d\n",huge_fd, size);	
        fprintf(stderr,"Error: Couldn't mmap huge page file. Perhaps it's too big?\n");
        return (0);
    }

    hugemem->fd     = huge_fd;
    hugemem->addr   = (void *) huge_addr;
    hugemem->length = size;
    return (1);
}

void
cp_hugemem_free( cp_hugemem* hugemem )
{
    (void)munmap( (void*)hugemem->addr, hugemem->length );
    (void)close( hugemem->fd );
}

#endif
#endif

