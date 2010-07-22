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

#include <stdio.h>
#include "xmltoken.h"
#include "pbpi.h"
#include "error.h"
#include "util.h"

int nextXMLToken(FILE *in, XMLToken *xmltoken)
{
	int i, c;

	static char whitespace[] = " \n\t\r";
	while( (c=fgetc(in))!= EOF && strchr(whitespace, c) );
	if ( c == EOF ){
		goto token_error;
	}
	else if ( c == '<' ){
		c = fgetc(in);
		if( c == '/' ){
			xmltoken->type = XML_KEY_END;
			c = fgetc(in);
		}
		else{
			xmltoken->type = XML_KEY_BEGIN;
		}
	}
	else{
		xmltoken->type = XML_VALUE;
	}

	xmltoken->token[0] = c;
	for(i=1; i<MAX_XML_TOKEN; i++){
		c=fgetc(in);
		if( c==EOF )
			break;
		else if (  c=='<' || c=='>' )
			break;
		else
			xmltoken->token[i] = c;
	}
	xmltoken->token[i] = '\0';
	if(   (c=='>' && xmltoken->type == XML_VALUE)
		||(c=='<' && xmltoken->type != XML_VALUE) ){
			goto token_error;
	}

	if( c == '<' ){
		ungetc(c, in);
	}
	
	return PBPI_SUCCESS;

token_error:
	xmltoken->token[0] = '\0';
	xmltoken->type = XML_ERROR;
	return PBPI_ERROR;
}

void dumpXMLToken(XMLToken *xmltoken)
{
	printf("XMLTOKEN: [type=%d] [value=%s]\n", xmltoken->type, xmltoken->token);
}

void copyXMLToken(XMLToken *dest, XMLToken *src)
{
	int len;
	dest->type = src->type;
	len = min2( strlen(src->token), MAX_XML_TOKEN);
	tokenSafeCopy( dest->token, MAX_XML_TOKEN, src->token);
	dest->token[len] = '\0';
}

static XMLToken	tokenStack[MAX_TOKEN_STACK_SIZE];
static int tokenStackSize = MAX_TOKEN_STACK_SIZE;
static int tokenStackTop  = 0;

//requires: xmltoken is nuo null
//effects: xmltoken is push to the top of the tatck and stack top increase 1
int tokenStackPush(XMLToken *xmltoken)
{
	//debug("push: %s", xmltoken->token);
	if( tokenStackTop < tokenStackSize ){
		copyXMLToken(&tokenStack[tokenStackTop++], xmltoken);
		return PBPI_SUCCESS;
	}
	else{
		warning("tokenStack overflow");
		return PBPI_ERROR;
	}
}

int tokenStackPop(XMLToken *xmltoken)
{
	//debug("pop: %s", xmltoken->token);
	if  (tokenStackTop > 0 ){
		copyXMLToken(xmltoken, &tokenStack[tokenStackTop--]);
		return PBPI_SUCCESS;
	}
	else{
		warning("tokenStack underflow");
		return PBPI_ERROR;
	}

}

void tokenStackPeek(XMLToken *xmltoken)
{
	copyXMLToken(xmltoken, &tokenStack[tokenStackTop-1]);
}

void getTokenSpace(XMLTokenSpace *tokenSpace)
{
	tokenSpace->list = tokenStack;
	tokenSpace->size = tokenStackTop;
}

void dumpTokenSpace(XMLTokenSpace *tokenSpace)
{
	int i;
	for(i=0; i<tokenSpace->size; i++){
		printf( "%s::", tokenSpace->list[i].token);
	}
	printf("\n");
}

static char tokenSpacePath[MAX_TOKEN_PATH_LEN + 1];
static int  tokenSpacePathLen = 0;
char *tokenSpace2String(XMLTokenSpace *tokenSpace)
{
	int i;

	tokenSpacePath[0] = '\0';
	tokenSpacePathLen = 0;
	for(i=0; i<tokenSpace->size; i++){
		tokenSpacePathLen += strlen(tokenSpace->list[i].token);
		if( tokenSpacePathLen >= MAX_TOKEN_PATH_LEN){
			bug("token space path length beyond the program limits\n");
		}
		strcat(tokenSpacePath, tokenSpace->list[i].token);
		strcat(tokenSpacePath, "::");
	}
	return tokenSpacePath;
}

