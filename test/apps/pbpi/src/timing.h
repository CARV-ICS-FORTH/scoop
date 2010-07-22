#ifndef _PBPI_TIME_H
#define	_PBPI_TIME_H


typedef struct NodeTime
{
	double	
		node_user_time,		//local node user time
		node_syst_time,		//local node system time
		node_cpu_time,		//local node cpu time, i.e. user_time + system_time	
		node_wall_time;		//local node wall time
}NodeTime;

typedef struct CommTime
{
	double	
		total_user_time,
		total_syst_time,
		total_cpu_time,
		total_wall_time,

		avg_user_time,
		avg_syst_time,
		avg_cpu_time,
		avg_wall_time,

		max_user_time,
		max_syst_time,
		max_cpu_time,
		max_wall_time,

		cpu_time,
		wall_time;
}CommTime;

double	timer_init( MPI_Comm comm );
double	timer_read( MPI_Comm comm, int myrank, FILE *fpout, char *info );

#endif

