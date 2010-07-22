#include <stdlib.h>
#include <malloc.h>

#define ALIGNMENT_SIZE 16

void *c_malloc(size_t size){
	return memalign(ALIGNMENT_SIZE, size);
}

void c_free(void *ptr){
	free(ptr);
}
