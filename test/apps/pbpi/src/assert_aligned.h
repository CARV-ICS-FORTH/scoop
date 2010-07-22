#ifndef _ASSERT_ALIGNED_H_
#define _ASSERT_ALIGNED_H_

typedef struct arg_data *arg_data_p;
typedef struct assertion_table *assertion_table_p;

void init_aligned(
	assertion_table_p table,
	unsigned short pos,
	const char *name
);

void set_aligned(
	assertion_table_p table,
	unsigned short pos,
	void *arg,
	unsigned long size
);
void set_debug_info(assertion_table_p table, unsigned int iter_no, char *fname);
void free_aligned(assertion_table_p table);
assertion_table_p create_aligned(unsigned short size);
void assert_aligned(assertion_table_p table);

#endif /*_ASSERT_ALIGNED_H_*/
