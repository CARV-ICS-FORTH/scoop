/** This software is part of the dissertation "High Performance, Bayesian-based Phylogenetic Inference Framework"
 *  by Xizhou Feng and is still under development.
 *  Copyright:
 *	Xizhou Feng
 *      Kirk Cameron
 *      Virginia Tech IP
 *  Contact:
 *	Xizhou Feng
 *	Department of Computer Science
 *	Virgnia Tech, Blacksburg, VA 24060
 *	Email: fengx@cs.vt.edu
 *	Phone: (540)231-9245
 */

#include <unistd.h>
#include <vec_types.h>
#include "pbpi.h"
#include "runsetting.h"
#include "dataset.h"
#include "util.h"
#include "mcmc.h"
#include "sumt.h"
#include "parallel.h"
#include "timing.h"
#include "tpc_common.h"
#include "tpc_ppe.h"
#include <assert.h>
#include "common.h"
#include "align_alloc.h"
#include "stlpc.h"

//global variables definition
int				g_Commsize								=	1,
				g_Myrank								=	0;
char			g_sRuncSettingFile[ MAX_FILE_NAME_LEN+1 ] = "pbpi.xml";
RunSetting		g_rs;
Dataset			g_ds;
FILE *			g_in;
ChainProcessMap g_map;

double			g_timing[8];
IMatrix			g_layout;
double			g_layout_width = 70.0;
double			g_layout_scale = 10.0;
extern	CommTime		commTime;

//this global variables are set by the command line
//and overwrittten the runsetting file
int				g_nchains_per_row					=	1;
int				g_ncols_per_chain					=	1;
char			g_dataset_name[MAX_FILE_NAME_LEN]	=	"pbpi.nex";
int				g_total_generation					=	10000;
unsigned int no_of_SPEs		= 0;//6;
unsigned int vectorized		= 0;
unsigned int use_SPEs		= 0;
unsigned int SPE_frg_lp0	= 0;//1360;
unsigned int SPE_frg_lp1	= 0;//1008;
unsigned int SPE_frg_lp2	= 0;//3632;

struct tpc_ds           *ds0[2];
struct tpc_ds           *ds1[2];
struct tpc_ds           *ds2[2];
struct tpc_ds           *ds_ppe;
struct tpc_ds           *ds[MAX_PROCS_NO];

struct loop_properties lp0p;
struct loop_properties lp1p;
struct loop_properties lp2p;

long double *tmp_lnL = NULL;
unsigned int total_lp2_sgms;

uint64_t     time_lp0 = 0;
uint64_t     time_lp1 = 0;
uint64_t     time_lp2 = 0;

void usage(char *prog)
{
	if (g_Myrank == HEAD_NODE){
		printf("usage: %s <run-setting-file>\n", prog);
	}
	exit(0);
}

int main(int argc, char *argv[])
{
    	unsigned short i;
	unsigned long	seeds[1];
	FILE *fp;

	fp = fopen("TPC_args", "r");
	if(fp == NULL){
		printf("File : \"TPC_args\" not found.\nTerminating ...\n");
		exit(0);
	}

	fscanf(fp, "%u\n", &use_SPEs);
	assert( use_SPEs == 0 || use_SPEs == 1 );
	if(!use_SPEs)
		printf("Application started in sequential mode.\n");
	else{
		printf("Application started in parallel mode.\n");

		fscanf(fp, "%u", &vectorized);
		assert( vectorized == 0 || vectorized == 1 );
		if(vectorized)
			printf("Vectorization enabled.\n");
		else
			printf("Vectorization disabled.\n");

		fscanf(fp, "%u", &no_of_SPEs);
		assert( no_of_SPEs > 0 && no_of_SPEs <= MAX_PROCS_NO );
		printf("Using #SPEs : %u\n",no_of_SPEs);

		fscanf(fp, "%u", &SPE_frg_lp0);
		assert( SPE_frg_lp0 > 0 && SPE_frg_lp0 < 1361 );
		printf("Loop 0 Iterations  : %u\n",SPE_frg_lp0);

		fscanf(fp, "%u", &SPE_frg_lp1);
		assert( SPE_frg_lp1 > 0 && SPE_frg_lp1 < 1009 );
		printf("Loop 1 Iterations  : %u\n",SPE_frg_lp1);

		fscanf(fp, "%u", &SPE_frg_lp2);
		assert( SPE_frg_lp2 > 0 && SPE_frg_lp2 < 3633 );
		printf("Loop 2 Iterations  : %u\n",SPE_frg_lp2);
	}

	fclose(fp);

	for(i=0; i<2; i++){
		ds0[i] = c_malloc(sizeof(struct tpc_ds));
		assert(ds0[i] != NULL);
		ds1[i] = c_malloc(sizeof(struct tpc_ds));
		assert(ds1[i] != NULL);
		ds2[i] = c_malloc(sizeof(struct tpc_ds));
		assert(ds2[i] != NULL);
		ds0[i]->vcted = vectorized;
		ds0[i]->start = 0;
		ds1[i]->vcted = vectorized;
		ds1[i]->start = 0;
		ds2[i]->vcted = vectorized;
		ds2[i]->start = 0;
	}
	ds_ppe = c_malloc(sizeof(struct tpc_ds));
	assert(ds_ppe != NULL);
	ds_ppe->vcted = vectorized;


	for(i=0; i<MAX_PROCS_NO; i++){
		ds[i] = c_malloc(sizeof(struct tpc_ds));
		assert(ds[i] != NULL);
		ds[i]->vcted = vectorized;
		ds[i]->start = 0;
	}

	if ( initMPIEnviornment(&argc, &argv ) )
	{
		if( argc >= 2 )
		{
			int nTmp;
			nTmp = min2( strlen(argv[1]), MAX_FILE_NAME_LEN );
			tokenSafeCopy(g_sRuncSettingFile,nTmp,  argv[1] );
			g_sRuncSettingFile[nTmp] = '\0';

			if( argc >= 6 )
			{
				nTmp = min2( strlen(argv[2]), MAX_FILE_NAME_LEN );
				tokenSafeCopy(g_dataset_name, nTmp,  argv[2] );
				g_total_generation	=	atoi( argv[3] );
				g_nchains_per_row	=	max2(1, atoi( argv[4] ));
				g_ncols_per_chain	=	max2(1, atoi( argv[5] ));
				printf("[%d]BENCH: file=%s ngen=%d %d %d\n",
					g_Myrank, g_dataset_name, g_total_generation, g_nchains_per_row, g_ncols_per_chain);

			}
		}

		g_timing[0]	=	timer_init( MPI_COMM_WORLD );					//start time

		showProgramInformation( );

		if ( readRunsetting_p( g_sRuncSettingFile, &g_rs ) )
		{
			if( argc >= 6 )
			{
				tokenSafeCopy(g_rs.sDatasetFile, MAX_FILE_NAME_LEN, g_dataset_name);
				g_rs.nDataset = 1;
				g_rs.fEnableDatasetParallelism = FALSE;
				g_rs.nMaxChainGenerations = g_total_generation;
				g_rs.nChainsPerGroup = g_nchains_per_row;
				g_rs.nProcessPerChain= g_ncols_per_chain;
			}
			if( chdir( g_rs.sWorkingDir ) < 0 )
				exitProgram( -1 );

			if ( buildChainProcessMap( &g_map, &g_rs ) )
			{
				int	indexDataset;

				CMatrix		g_cmDatasetFileList;
				initCMatrix( &g_cmDatasetFileList );
				allocCMatrix( &g_cmDatasetFileList, g_rs.nDataset, MAX_FILE_NAME_LEN );

				if( g_rs.fEnableBatchProcessing )
				{
					distributeDataset_p( &g_rs, &g_cmDatasetFileList );
				}
				setRNGSeed_p( &g_rs );

				showChainProcessMap( );
				//if( g_map.rankMPIWorld == HEAD_NODE )
				//{
				//	writelog(g_rs.sLogFile, "%10s%-20s %6s %6s %6s %10s %10s\n",
				//							" ",
				//							"dataset",
				//							"npgrid",
				//							"nrows",
				//							"ncols",
				//							"cputime",
				//							"walltime");
				//}

				g_timing[1]	=	timer_read( MPI_COMM_WORLD, g_map.rankMPIWorld, stdout, "Build Chain Map" );					//prepare time

				for( indexDataset=0; indexDataset<g_map.numLocalDataset; indexDataset++)
				{
					g_timing[2]	=	timer_init( g_map.commBootstrap);			//before read dataset

					if( g_rs.fEnableBatchProcessing )
					{
						/*printf("Batch processing is enabled.\n");*/
						tokenSafeCopy( g_rs.sDatasetFile, MAX_FILE_NAME_LEN, g_cmDatasetFileList.data[ indexDataset ]);
					}

					if ( readDataset_p( &g_rs, &g_ds ) )
					{
						int	indexBootstrap;
						compressDataset( &g_ds );
						allocIMatrix( &g_layout, g_ds.nTaxa * 2, (int)(g_layout_width + 1) );
						g_timing[3]	=	timer_read( g_map.commBootstrap, g_map.rankBootstrap, stdout, "Read Dataset" );			//after read dataset

						if(use_SPEs)
							tpc_init(no_of_SPEs);

						fix_lp0_pr(g_ds.nPattern, SPE_frg_lp0);
						fix_lp1_pr(g_ds.nPattern, SPE_frg_lp1);
						fix_lp2_pr(g_ds.nPattern, SPE_frg_lp2);
						ds0[0]->end = lp0p.full_iter_size;
						ds1[0]->end = lp1p.full_iter_size;
						ds2[0]->end = lp2p.full_iter_size;
						if(tmp_lnL == NULL){
						    total_lp2_sgms = lp2p.full_iterations+lp2p.mod_iterations;
						    if(lp2p.ppe_iterations)
						    	++total_lp2_sgms;
						    tmp_lnL = c_malloc(total_lp2_sgms*sizeof(long double));
						    assert(tmp_lnL != NULL);
						}
						ds0[1]->end = lp0p.mod_iter_size;
						ds1[1]->end = lp1p.mod_iter_size;
						ds2[1]->end = lp2p.mod_iter_size;
						if(lp2p.ppe_iterations){
							ds_ppe->start = lp2p.full_iterations*lp2p.full_iter_size + lp2p.mod_iterations*lp2p.mod_iter_size;
							ds_ppe->end = ds_ppe->start + lp2p.ppe_iterations;
						}

						print_lp_properties(lp0p, "LOOP 0");
						print_lp_properties(lp1p, "LOOP 1");
						print_lp_properties(lp2p, "LOOP 2");

						for( indexBootstrap=g_map.indexDatasetGroup; indexBootstrap<g_rs.nBootstrap; indexBootstrap+=g_map.numBootstrapGroup)
						{
							int	indexRun;
							MPI_Barrier( g_map.commBootstrap );

							//bootstrap dataset except the first one
							if( indexBootstrap != 0 )
							{
								resampleDataMatrix( &g_ds, g_rs.dBootstrapSamplingKnife, g_rs.dBootstrapSamplingRatio );
							}

							//bootstrap data here
							for( indexRun=g_map.indexRunGroup; indexRun<g_rs.nRuns; indexRun+=g_map.numRunGroup)
							{

								MPI_Barrier( g_map.commRun );
								if( g_rs.nBootstrap <= 1 )
								{
									if( g_rs.nRuns <= 1)
										message_p( "Processing Dataset %s", g_rs.sDatasetFile);
									else
										message_p( "Processing Dataset %s \t run# = %d", g_rs.sDatasetFile, indexRun);
								}
								else
								{
									if( g_rs.nRuns <= 1)
										message_p( "Bootstrapping Dataset %s \t repeat = %d", g_rs.sDatasetFile, indexBootstrap);
									else
										message_p( "Processing Dataset %s \t repeat = %d \t run# = %d", g_rs.sDatasetFile, indexBootstrap, indexRun);

								}
								if(g_rs.fRunMCMC)
								{
									MPI_Barrier( g_map.commGrid );
									g_timing[4]	=	timer_init( g_map.commGrid );
									runMarkovChains(indexDataset, indexBootstrap, indexRun);
									MPI_Barrier( g_map.commGrid);
									g_timing[5]	=	timer_read( g_map.commGrid, g_map.rankGrid, stdout, "MCMC Sampling" );
									if( g_map.rankGrid == HEAD_NODE )
									{
										writelog(g_rs.sLogFile, "%-30s %6d %6d %6d %6d %6d %10.2f %10.2f\n",
											g_rs.sDatasetFile,
											g_rs.nMaxChainGenerations,
											g_map.npGrid, g_map.numRows, g_map.numCols,
											indexRun,
											commTime.max_cpu_time, commTime.max_wall_time);
									}
									message_p("Timing 2 ( MCMC ) = %10.3f", g_timing[5] - g_timing[4]);
								}

								if(g_rs.fRunSumt)
								{
									g_timing[6]	=	timer_init( g_map.commGrid );
									summaryTree_p( indexDataset, indexBootstrap, indexRun );
									g_timing[7]	=	timer_read( g_map.commGrid, g_map.rankGrid, stdout, "Summary Trees" );
									message_p("Timing 3 ( SUMT ) = %10.3f", g_timing[6] - g_timing[5]);
								}
								fflush(stdout);
							}
						}
						freeDataset( &g_ds );
						freeIMatrix( &g_layout );
					}
					else
					{
						message_p("ERROR: read dataset failed");
						exitProgram( -4 );
					}
				}

				freeCMatrix( &g_cmDatasetFileList );
			}
			else
			{
				message_p("ERROR: build parallel topology failed");
				exitProgram( -3 );
			}
		}
		else
		{
			message_p("ERROR: reading runsetting failed");
			exitProgram( -2 );

		}
	}
	else
	{
		message_p("ERROR: init MPI enviornment failed");
		exitProgram( -1 );

	}

	for(i=0; i<MAX_PROCS_NO; i++)
		c_free(ds[i]);

	if(tmp_lnL != NULL){
	    c_free(tmp_lnL);
	}

	tpc_shutdown();
	print_loop_profiling();
	tpc_print_stats(stdout);
	exitProgram( 0 );

	return 0;
}

