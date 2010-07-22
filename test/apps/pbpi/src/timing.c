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

#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#include "timing.h"

#ifndef HEAD_NODE
#define HEAD_NODE 0
#endif

struct rusage _rusage[2];
double		  _time[2];
int			  _timer[2]={0, 1};	
CommTime		commTime;

//double getWallTime()
//{
//	static char *uptimefile="/proc/uptime";
//	FILE *in;
//	double t;
//
//	in = fopen( uptimefile, "r" );
//	if( in )
//	{
//		fscanf(in, "%f", &t);
//		fclose(in);
//	}
//	else
//	{
//		t = time( NULL );
//	}
//	return t;
//}

double getWallTime()
{
	//double t;
	//t = time( NULL );
	//return t;
	return MPI_Wtime();
}
double timer_init( MPI_Comm comm )
{
	MPI_Barrier( comm );
	getrusage( RUSAGE_SELF, _rusage + _timer[0] );
	_time[ _timer[0] ] = getWallTime();
	return 0.0;
}

double timer_read( MPI_Comm comm, int myrank, FILE *fpout, char *info )
{
	NodeTime		localTime, *globalTime;
	int				np, index, t0, t1;

	MPI_Barrier( comm );
	getrusage( RUSAGE_SELF, _rusage + _timer[1] );
	_time[ _timer[1] ] = getWallTime();

	MPI_Comm_size( comm, &np );
	globalTime = (NodeTime *)malloc( sizeof(NodeTime) * np );

	t0 = _timer[0];  t1 = _timer[1];

	localTime.node_user_time = 
		(_rusage[t1].ru_utime.tv_sec  - _rusage[t0].ru_utime.tv_sec)	+ 
		(_rusage[t1].ru_utime.tv_usec - _rusage[t0].ru_utime.tv_usec) * 1.0e-6;
	localTime.node_syst_time = 
		(_rusage[t1].ru_stime.tv_sec  - _rusage[t0].ru_stime.tv_sec)		+ 
		(_rusage[t1].ru_stime.tv_usec - _rusage[t0].ru_stime.tv_usec) * 1.0e-6;
	localTime.node_cpu_time  = localTime.node_user_time + localTime.node_syst_time;
	localTime.node_wall_time = _time[t1] - _time[t0];
	_timer[1] = t0; _timer[0] = t1;

	MPI_Gather(&localTime, sizeof(NodeTime), MPI_CHAR, globalTime, sizeof(NodeTime), MPI_CHAR, 0, comm);

	if( myrank == HEAD_NODE )
	{
		commTime.total_user_time = 0;
		commTime.total_syst_time = 0;
		commTime.total_cpu_time  = 0;
		commTime.total_wall_time = 0;

		commTime.max_user_time = 0;
		commTime.max_syst_time = 0;
		commTime.max_cpu_time  = 0;
		commTime.max_wall_time = 0;

		for(index=0; index<np; index++)
		{
			commTime.total_user_time += globalTime[ index ].node_user_time;
			commTime.total_syst_time += globalTime[ index ].node_syst_time;			
			commTime.total_cpu_time  += globalTime[ index ].node_cpu_time;
			commTime.total_wall_time += globalTime[ index ].node_wall_time;

			if(  globalTime[ index ].node_user_time > commTime.max_user_time)
				commTime.max_user_time =  globalTime[ index ].node_user_time;

			if(  globalTime[ index ].node_syst_time > commTime.max_syst_time )
				commTime.max_syst_time =  globalTime[ index ].node_syst_time;

			if(  globalTime[ index ].node_wall_time > commTime.max_wall_time)
				commTime.max_wall_time =  globalTime[ index ].node_wall_time;

			if(  globalTime[ index ].node_cpu_time > commTime.max_cpu_time)
				commTime.max_cpu_time =  globalTime[ index ].node_cpu_time;
		}

		commTime.avg_user_time = commTime.total_user_time / np;
		commTime.avg_syst_time = commTime.total_syst_time / np;
		commTime.avg_cpu_time  = commTime.total_cpu_time  / np;
		commTime.avg_wall_time = commTime.total_wall_time / np;

		commTime.cpu_time		  = commTime.max_cpu_time;
		commTime.wall_time		  = commTime.max_wall_time;

		fprintf(fpout, "\nTime ( %s ) : %10.2f %10.2f\n", info, commTime.cpu_time, commTime.wall_time);
		fprintf(fpout, "==================================================\n");
		fprintf(fpout, "%6s %10s %10s %10s %10s\n",
			"Rank",
			"User",
			"Sys",
			"CPU",
			"Wtime");
		fprintf(fpout, "--------------------------------------------------\n");
		for(index=0; index<np; index++)
		{
			fprintf(fpout, "%6d %10.2f %10.2f %10.2f %10.2f\n",
				index,	
				globalTime[ index ].node_user_time, 
				globalTime[ index ].node_syst_time, 
				globalTime[ index ].node_cpu_time,
				globalTime[ index ].node_wall_time);
		}
		fprintf(fpout, "--------------------------------------------------\n");
		fprintf(fpout, "%6s %10.2f %10.2f %10.2f %10.2f\n",
			"Total:",	
			commTime.total_user_time, 
			commTime.total_syst_time, 
			commTime.total_cpu_time, 
			commTime.total_wall_time);
			
		fprintf(fpout, "%6s %10.2f %10.2f %10.2f %10.2f\n",
			"Avg:",	
			commTime.avg_user_time, 
			commTime.avg_syst_time, 
			commTime.avg_cpu_time,  
			commTime.avg_wall_time);

		fprintf(fpout, "%6s %10.2f %10.2f %10.2f %10.2f\n",
			"Max:",	
			commTime.max_user_time, 
			commTime.max_syst_time, 
			commTime.max_cpu_time,  
			commTime.max_wall_time );
		fprintf(fpout, "==================================================\n\n");
		fflush(fpout);
	}	
	//MPI_Bcast ( &(commTime.max_wall_time), 1, MPI_DOUBLE, 0, comm); 

	return commTime.max_wall_time;
}

//int main( int argc, char *argv[])
//{
//	int		myrank;
//	double	wall_time;
//
//	MPI_Init( &argc, &argv );
//	MPI_Comm_rank( MPI_COMM_WORLD, &myrank);
//
//	pbpi_timer_init( MPI_COMM_WORLD );
//
//	wall_time = pbpi_timer_read( MPI_COMM_WORLD, myrank, stdout, "Timing Test" );
//
//	MPI_Finalize();
//	return 0;
//}

