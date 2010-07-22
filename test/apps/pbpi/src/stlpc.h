#ifndef _STLPC_H_
#define _STLPC_H_

struct loop_properties{
	unsigned int full_iterations;
	unsigned int full_iter_size;
	unsigned int full_distance;
	unsigned int full_fixed;
	unsigned int full_sroot_fixed;

	unsigned int mod_iterations;
	unsigned int mod_iter_size;
	unsigned int mod_distance;
	unsigned int mod_fixed;
	unsigned int mod_sroot_fixed;

	unsigned int ppe_iterations;

        unsigned int *task_ids;


} __attribute__ ((aligned(16)));

void fix_lp0_pr(unsigned int total, unsigned int requested);
void fix_lp1_pr(unsigned int total, unsigned int requested);
void fix_lp2_pr(unsigned int total, unsigned int requested);
void print_lp_properties(struct loop_properties lpp, char *loop_name);
void print_loop_profiling ( void ) ;

#endif
