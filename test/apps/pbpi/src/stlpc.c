#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include "common.h"
#include "stlpc.h"
#include "align_alloc.h"

extern struct loop_properties	lp0p;
extern struct loop_properties	lp1p;
extern struct loop_properties	lp2p;

extern unsigned int		no_of_SPEs;

extern uint64_t			time_lp0;
extern uint64_t			time_lp1;
extern uint64_t			time_lp2;
extern uint64_t     		entries_lp0;
extern uint64_t     		entries_lp1;
extern uint64_t     		entries_lp2;

void optimize_lp2_pr(unsigned int *requested);
void ldbl_heuristic( unsigned int total,
		unsigned int *req, unsigned int *req_it,
		unsigned int *mod, unsigned int *mod_it,
		unsigned int upper_limit
);
void align_heuristic( unsigned int shift_bits,
		unsigned int *req, unsigned int *req_it,
		unsigned int *mod, unsigned int *mod_it,
		unsigned int *ppe_iter, unsigned int upper_limit
);

void fix_lp0_pr(unsigned int total, unsigned int requested){
	lp0p.full_iter_size	= requested;
	ldbl_heuristic( total,
			&lp0p.full_iter_size, &lp0p.full_iterations,
			&lp0p.mod_iter_size, &lp0p.mod_iterations,
			MAX_LP0_IT
	);
	lp0p.full_distance	= lp0p.full_iter_size << 2;
	lp0p.full_fixed		= lp0p.full_distance << 3;
	lp0p.mod_distance	= lp0p.mod_iter_size << 2;
	lp0p.mod_fixed		= lp0p.mod_distance << 3;
	lp0p.full_sroot_fixed	= 0;
	lp0p.mod_sroot_fixed	= 0;
	lp0p.ppe_iterations	= 0;
        lp0p.task_ids           = memalign( 16, (lp0p.mod_iterations+ lp0p.full_iterations)  *sizeof(unsigned int) );
	// Invariant : Don't loose any dataset iterations
	assert(lp0p.full_iter_size*lp0p.full_iterations + lp0p.mod_iter_size*lp0p.mod_iterations == total);
}

void fix_lp1_pr(unsigned int total, unsigned int requested){
	lp1p.full_iter_size	= requested;
	ldbl_heuristic( total,
			&lp1p.full_iter_size, &lp1p.full_iterations,
			&lp1p.mod_iter_size, &lp1p.mod_iterations,
			MAX_LP1_IT
	);
	lp1p.full_distance	= lp1p.full_iter_size << 2;
	lp1p.full_fixed		= lp1p.full_distance << 3;
	lp1p.mod_distance	= lp1p.mod_iter_size << 2;
	lp1p.mod_fixed		= lp1p.mod_distance << 3;
	lp1p.full_sroot_fixed	= 0;
	lp1p.mod_sroot_fixed	= 0;
	lp1p.ppe_iterations		= 0;

        lp1p.task_ids           = memalign( 16, (lp1p.mod_iterations+ lp1p.full_iterations) *sizeof(unsigned int) );

	// Invariant : Don't loose any dataset iterations
	assert(lp1p.full_iter_size*lp1p.full_iterations + lp1p.mod_iter_size*lp1p.mod_iterations == total);
}

void fix_lp2_pr(unsigned int total, unsigned int requested){
	lp2p.full_iter_size	= requested;
	ldbl_heuristic( total,
			&lp2p.full_iter_size, &lp2p.full_iterations,
			&lp2p.mod_iter_size, &lp2p.mod_iterations,
			MAX_LP2_IT
	);

	align_heuristic( 2,
			&lp2p.full_iter_size, &lp2p.full_iterations,
			&lp2p.mod_iter_size, &lp2p.mod_iterations,
			&lp2p.ppe_iterations, MAX_LP2_IT
	);

	lp2p.full_distance	= lp2p.full_iter_size;
	lp2p.full_fixed		= lp2p.full_iter_size << 2;
	lp2p.full_sroot_fixed	= lp2p.full_iter_size << 5;
	lp2p.mod_distance	= lp2p.mod_iter_size;
	lp2p.mod_fixed		= lp2p.mod_iter_size << 2;
	lp2p.mod_sroot_fixed	= lp2p.mod_iter_size << 5;

        lp2p.task_ids           = memalign( 16, (lp2p.mod_iterations+ lp2p.full_iterations) *sizeof(unsigned int) );

	// Invariant : Don't loose any dataset iterations. Keep alignment.
	assert(lp2p.full_iter_size*lp2p.full_iterations + lp2p.mod_iter_size*lp2p.mod_iterations + lp2p.ppe_iterations == total);
	assert(lp2p.full_iter_size%4 == 0 && lp2p.mod_iter_size%4 == 0);
}

void print_lp_properties(struct loop_properties lpp, char *loop_name){
	printf("Printing properties for : {%s}\n",loop_name);
	printf("Full iterations = [%u]\n", lpp.full_iterations);
	printf("Mod iterations = [%u]\n", lpp.mod_iterations);
	printf("Full iteration size = [%u]\n", lpp.full_iter_size);
	printf("Mod iteration size = [%u]\n", lpp.mod_iter_size);
	printf("Full distance = [%u]\n", lpp.full_distance);
	printf("Full fixed = [%u]\n", lpp.full_fixed);
	printf("Full sroot fixed = [%u]\n", lpp.full_sroot_fixed);
	printf("Mod distance = [%u]\n", lpp.mod_distance);
	printf("Mod fixed = [%u]\n", lpp.mod_fixed);
	printf("Mod sroot fixed = [%u]\n\n", lpp.mod_sroot_fixed);
	printf("Unaligned PPE Iterations = [%u]\n\n", lpp.ppe_iterations);
}

void print_loop_profiling ( void ) {
	printf("Loop 1 Time (~ticks) : %llu\n", time_lp0);
	printf("Loop 2 Time (~ticks) : %llu\n", time_lp1);
	printf("Loop 3 Time (~ticks) : %llu\n", time_lp2);
	printf("Loop 1 Time (sec) : %lf\n", (double)time_lp0/(double)TIMER_FREQ);
	printf("Loop 2 Time (sec) : %lf\n", (double)time_lp1/(double)TIMER_FREQ);
	printf("Loop 3 Time (sec) : %lf\n", (double)time_lp2/(double)TIMER_FREQ);
}

void optimize_lp2_pr(unsigned int *requested){
	unsigned ir = *requested, i;
	if(*requested < 5){
		*requested = 4;
		goto escp0;
	}
	*requested >>= 2;
	*requested <<= 2;
escp0:
	if(ir % 4){
		for(i=0; i<80; i++)
			fprintf(stderr, "*");
	    fprintf(stderr, "\nWARNING : performance issue at loop2 size transfer.\n");
	    fprintf(stderr, "Size given : \"%d\" has been auto adjasted to : \"%d\".\n", ir, *requested);
	    fprintf(stderr, "Please use sizes that can be divided by 4.\n");
	    for(i=0; i<80; i++)
	    	fprintf(stderr, "*");
	    fprintf(stderr, "\n");
	}
}

void find_limits(unsigned int number, unsigned int *floor, unsigned int *ceil){
	unsigned int fl_tmp = number / no_of_SPEs;
	*floor = fl_tmp * no_of_SPEs;
	*ceil = (fl_tmp+1) * no_of_SPEs;
}

void align_heuristic( unsigned int shift_bits,
		unsigned int *req, unsigned int *req_it,
		unsigned int *mod, unsigned int *mod_it,
		unsigned int *ppe_iter, unsigned int upper_limit
){
	unsigned int min = ((*req) < (*mod)) ? (*req) : (*mod);
	unsigned int req_dist;
	unsigned int mod_dist;
	unsigned int rem_iter;
	unsigned int total_iter = *req_it + *mod_it;

	if(min == 0)
	    min = *req;

	min >>= shift_bits;
	min <<= shift_bits;

	req_dist = (*req) - min;
	mod_dist = (*mod) - min;

	rem_iter = req_dist*(*req_it) + mod_dist*(*mod_it);

	if(min + (1<<shift_bits) < upper_limit){
		*req = min + (1<<shift_bits);
		*req_it = rem_iter / (1<<shift_bits);
		*mod = min;
		*mod_it = total_iter - *req_it;
		*ppe_iter = rem_iter % (1<<shift_bits);
	}
	else{
		*req = min;
		*req_it = total_iter;
		*mod = 0;
		*mod_it = 0;
		*ppe_iter = rem_iter;
	}
}

void ldbl_heuristic( unsigned int total,
		unsigned int *req, unsigned int *req_it,
		unsigned int *mod, unsigned int *mod_it,
		unsigned int upper_limit
){

	if(*req * no_of_SPEs >= total){
		unsigned int fixed_req;
		unsigned int fixed_mod;

		fixed_req = total/no_of_SPEs;
		fixed_mod = total%no_of_SPEs;
		if(fixed_mod){
			*req = fixed_req + 1;
			*req_it = fixed_mod;
			*mod = fixed_req;
			*mod_it = no_of_SPEs - fixed_mod;
		}
		else{
			*req = fixed_req;
			*req_it = no_of_SPEs;
			*mod = 0;
			*mod_it = 0;
		}
	}
	else{
		unsigned int iter_no = total / *req;
		unsigned int tmp = (total % *req) ? 1 : 0;
		unsigned int floor, floor_dist, floor_it, floor_mod, floor_flag;
		unsigned int ceil, ceil_dist, ceil_it, ceil_mod, ceil_flag;
		unsigned int selected;

		iter_no += tmp;

		find_limits(iter_no, &floor, &ceil);
		floor_dist = iter_no - floor;
		ceil_dist = ceil - iter_no;
		floor_it = total / floor;
		floor_mod = total % floor;
		floor_flag = (floor_mod) ? 1 : 0;
		ceil_it = total / ceil;
		ceil_mod = total % ceil;
		ceil_flag = (ceil_mod) ? 1 : 0;

		if(floor_it + tmp > upper_limit){
			if(ceil_mod){
				*req = ceil_it + 1;
				*req_it = ceil_mod;
				*mod = ceil_it;
				*mod_it = ceil- ceil_mod;
			}
			else{
				*req = ceil_it;
				*req_it = ceil;
				*mod = 0;
				*mod_it = 0;
			}
			return;
		}

		if(ceil_dist < floor_dist){
			if(ceil_mod){
				*req = ceil_it + 1;
				*req_it = ceil_mod;
				*mod = ceil_it;
				*mod_it = ceil- ceil_mod;
			}
			else{
				*req = ceil_it;
				*req_it = ceil;
				*mod = 0;
				*mod_it = 0;
			}
		}
		else{
			if(floor_mod){
				*req = floor_it + 1;
				*req_it = floor_mod;
				*mod = floor_it;
				*mod_it = floor- floor_mod;
			}
			else{
				*req = floor_it;
				*req_it = floor;
				*mod = 0;
				*mod_it = 0;
			}
		}
	}
}
