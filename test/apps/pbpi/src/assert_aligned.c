#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "assert_aligned.h"

#define MODE_64_ENABLED_

struct arg_data{
	void *arg;
	unsigned long size;
	unsigned long iterNo;
	char *arg_name;
};

struct assertion_table{
	struct arg_data *data;
	unsigned short args;
	unsigned int   iter_no;
	char *func_name;
};

void init_aligned(
	assertion_table_p table,
	unsigned short pos,
	const char *name
){
	table->data[pos].arg_name = (char *)name;
	table->data[pos].iterNo = 0;
}

void set_aligned(
	assertion_table_p table,
	unsigned short pos,
	void *arg,
	unsigned long size
){
	table->data[pos].arg = arg;
	table->data[pos].size = size;
}

void set_debug_info(assertion_table_p table, unsigned int iter_no, char *fname){
    table->iter_no = iter_no;
    table->func_name = fname;
}

void free_aligned(assertion_table_p table){
	assert(table != NULL && table->data != NULL);
	free(table->data);
	free(table);
}

assertion_table_p create_aligned(unsigned short size){
	assertion_table_p newTable = NULL;

	newTable = malloc(sizeof(struct assertion_table));
	assert(newTable != NULL);

	newTable->args = size;
	newTable->data = malloc(size*sizeof(struct arg_data));
	assert(newTable->data != NULL);

	return newTable;
}

void assert_aligned(assertion_table_p table){
	int cnt;
	unsigned char flag = 0;
#ifdef MODE_64_ENABLED
	unsigned long long upperBits;
	unsigned long long mask = 0xFFFF0000;
	upperBits = (unsigned long long)table->data[0].arg & mask;
#endif
	for(cnt = 0; cnt<table->args; cnt++){
#ifdef MODE_64_ENABLED
		if(upperBits != (unsigned long long)table->data[cnt].arg & mask){
			fprintf(stderr, "ARGUMENT 32 UPPER BITS ADDRESS ERROR.\n");
			fprintf(stderr, "Argument %s\n", table->data[cnt].arg_name);
			fprintf(stderr, "Address %p\n", table->data[cnt].arg);
			flag = 1;
		}
#endif
		if((unsigned long)table->data[cnt].arg % 16 != 0){
			fprintf(stderr, "ADDRESS ALIGMENT ERROR.\n");
			fprintf(stderr, "\tAt function | loop : \"%s\"\n",table->func_name);
			fprintf(stderr, "\tAt argument \"%s\":[%p]\n",table->data[cnt].arg_name, table->data[cnt].arg);
//			fprintf(stderr, "\tAt Iteretion :%lu\n",table->data[cnt].iterNo);
			fprintf(stderr, "\tAt Iteretion :%lu\n",table->iter_no);
			flag = 1;
		}
		if(table->data[cnt].size % 16 != 0){
			fprintf(stderr, "SIZE ALIGMENT ERROR.\n");
			fprintf(stderr, "\tAt function | loop : \"%s\"\n",table->func_name);
			fprintf(stderr, "\tAt argument \"%s\": size [%lu]\n",table->data[cnt].arg_name, table->data[cnt].size);
//			fprintf(stderr, "\tAt Iteretion :%lu\n", table->data[cnt].iterNo);
			fprintf(stderr, "\tAt Iteretion :%lu\n",table->iter_no);
			flag = 2;
		}
		if(flag == 1)
			exit(1);
		table->data[cnt].iterNo++;
	}
}
