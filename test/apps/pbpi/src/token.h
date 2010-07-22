#ifndef _PBPI_TOKEN_H
#define _PBPI_TOKEN_H

#include <stdio.h>

#define	MAX_NEXUS_TOKEN_LEN	1023


typedef enum NEXUSTokenType
{
	TOK_STRING=0,
    TOK_COMMENT_START=1,
    TOK_COMMENT_STOP=2,
    TOK_QUOTATION_START=3,
    TOK_QUOTATION_STOP=4,
    TOK_ASSIGN=5,
    TOK_SEMICOLON=6,
    TOK_NEWLINE=7,
	TOK_COMMA=8,
	TOK_COLON=9,
	TOK_LEFT_PARENTHESIS=10,
	TOK_RIGHT_PARENTHESIS=11,
    TOK_EOF=12,
	TOK_ERROR
} NEXUSTokenType;

typedef struct NEXUSToken
{
	NEXUSTokenType	type;
	char			token[MAX_NEXUS_TOKEN_LEN + 1];
}NEXUSToken;

NEXUSToken *nextNEXUSToken( FILE *in );
NEXUSToken *peekNEXUSToken( FILE *in );
int searchNEXUSToken( FILE *in, char *keyword );
NEXUSToken *_nextToken( FILE *in );

#endif	//_PBPI_TOKEN_H

