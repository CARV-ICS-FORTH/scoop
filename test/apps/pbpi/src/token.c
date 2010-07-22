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
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "error.h"
#include "util.h"
#include "pbpi.h"

static long	_nLine=0;	//current number of lines
static long	_nColm=0;	//current column in the line
static long _nChar=0;	//total chars were read

int getChar(FILE *in)
{
	int c;
	c = fgetc(in);
	if( c != EOF ){
		_nChar++;
		if( c=='\n' ){ 
			_nLine++; 
			_nColm = 0;
		}else{
			_nColm = 0;
		}
	}
	return c;
}


static char NEXUSTokenSeperators[] = " \t\r\n;=\',:[()";
static char szWhiteSpace[] = " \t\r\0";
static NEXUSToken	_nexusToken;
static char _lastchar = '\0';
static int  _nexustokenlen = 0;	
static char *_tokenbuf;

NEXUSToken *_nextToken( FILE *in )
{

	static NEXUSToken *tok = &_nexusToken;

	//step 1: init current token
	_nexusToken.token[0] = '\0';
	_nexusToken.type     = TOK_ERROR;
	_tokenbuf = _nexusToken.token;

	//step 2: skip whitespaces
	while( ( _lastchar = getChar(in) ) && strchr(szWhiteSpace, _lastchar) ) ;

	//step 3: determine token type
	switch (_lastchar) {
	case EOF:							//reached the end of the input stream
		tok->type  = TOK_EOF;
		goto end_token_read;

	case '\n':							//reached a new line, command from stdin ends with '\n'
		tok->type = TOK_NEWLINE;
		_tokenbuf[0] = '\\';
		_tokenbuf[1] = 'n';
		_tokenbuf[2] = '\0';
		goto end_token_read;

	case ';':							//finished current command
		tok->type = TOK_SEMICOLON;
		_tokenbuf[0] = ';';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case '=':							//key-value(s) assignment
		tok->type = TOK_ASSIGN;
		_tokenbuf[0] = '=';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case '[':							//start a new comment
		tok->type = TOK_COMMENT_START;
		_tokenbuf[0] = '[';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case ']':							//stop previous comment
		tok->type = TOK_COMMENT_STOP;
		_tokenbuf[0] = ']';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case ':':							//tree branch length
		tok->type = TOK_COLON;
		_tokenbuf[0] = ':';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case ',':							//multiple parts
		tok->type = TOK_COMMA;
		_tokenbuf[0] = ',';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case '(':							//a new tree node or group of values
		tok->type = TOK_LEFT_PARENTHESIS;
		_tokenbuf[0] = '(';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case ')':							//close current tree node or group of values
		tok->type = TOK_RIGHT_PARENTHESIS;
		_tokenbuf[0] = ')';
		_tokenbuf[1] = '\0';
		goto end_token_read;

	case '\'':							//quoted token
		tok->type = TOK_STRING;;
		for (_nexustokenlen = 0; _nexustokenlen < MAX_NEXUS_TOKEN_LEN - 1 ; _nexustokenlen++) {
			_lastchar = getChar ( in );
			if (_lastchar == '\'') {
				_lastchar = getChar ( in );
				if (_lastchar == '\'') {
					_tokenbuf[_nexustokenlen] = _lastchar;
					continue;
				}
				else {
					_tokenbuf[_nexustokenlen] = '\0';
					break;
				}
			}
			else if (_lastchar == EOF) {
				_tokenbuf[_nexustokenlen] = '\0';
				break;
			}
			else
				_tokenbuf[_nexustokenlen] = _lastchar;
		}
		_tokenbuf[_nexustokenlen] = '\0';
		if (_lastchar != EOF && _lastchar != '\n')
			ungetc ( _lastchar, in );
		goto end_token_read;

	default:
		break;
	}
	
	//step 4: deal with string token. i.e. read chars from input stream until another seperator occurs
    tok->type = TOK_STRING;
	_tokenbuf[0] = _lastchar;
	for (_nexustokenlen = 1; _nexustokenlen < MAX_NEXUS_TOKEN_LEN - 1; _nexustokenlen++) {
		_lastchar = getChar ( in );
		if (strchr (NEXUSTokenSeperators, _lastchar) || _lastchar == EOF)
			break;
		else   
			_tokenbuf[_nexustokenlen] = _lastchar;
	}
    if (_lastchar != EOF )
        ungetc (_lastchar, in);
    _tokenbuf[_nexustokenlen] = '\0';

end_token_read:
	//debug( "Nexus Token: (type=%d) (token=%s)\n", _nexusToken.type, _nexusToken.token);
    return	&_nexusToken;
}

void skipComment ( FILE *in )
{
    while ((_lastchar = getChar ( in )) != ']' && _lastchar != EOF);
    if (_lastchar != EOF && _lastchar != ']')
        ungetc (_lastchar, in);
}

NEXUSToken *nextNEXUSToken( FILE *in )
{
	NEXUSToken *pNEXUSToken;

	while ( 1 ){
		pNEXUSToken = _nextToken( in );
		//skip comments
		if ( pNEXUSToken->type == TOK_COMMENT_START )
			skipComment( in );
		//skip new line
		else if( pNEXUSToken->type == TOK_NEWLINE )
			continue;
		else
			break;
	}
	return pNEXUSToken;
}


//requires: none
//effects:	keeping get next token until the keyword is found
//returns:
//	0:	keyword is not found
//	1:	keyword is found
int searchNEXUSToken( FILE *in, char *keyword )
{
	NEXUSToken *pNEXUSToken;
	while(1)
	{
		pNEXUSToken = nextNEXUSToken( in );
		if ( tokenMatch(pNEXUSToken->token, keyword, TRUE) )
			return 1;
	}
	return 0;
}


NEXUSToken *peekNEXUSToken( FILE *in )
{
	int i=0;
	NEXUSToken *pNEXUSToken;

	//read the token
	pNEXUSToken = nextNEXUSToken( in );

	//putback token in reversed order
	for(i=strlen(pNEXUSToken->token)-1; i>=0; i--)
		ungetc (pNEXUSToken->token[i], in);

	return pNEXUSToken;
}
//
//char *subst_whitespace(char *sz)
//{
//	if(!sz) return NULL;
//	while(*sz!='\0') 
//	{
//		if(*sz==' ') *sz='_';
//		if(*sz=='\t') *sz='_';
//		sz++;
//	}
//	return sz;
//}
//
//char	UpperCase(char c)
//{
//	if( c>='a' && c<='z' )
//		return  c - 'a' + 'A';
//	else
//		return c;
//}
//
////to_uppercase
////parameter:
////	s:			the string to be capitalized
////return:		
////	pointer to the string
////requires:
////	s must be end with '\0'
////modifies:
////	s has been captilized
//char *to_uppercase(char *s)
//{
//	char *p=s;
//
//	//check the parameter
//	if(!s)	
//		return NULL;
//
////debug("orignial: %s", s);
//	//change each letter to uppercase
//	while (*p !='\0')
//	{
//		if( isalpha(*p) )
//			*p= toupper(*p);
//		//*p = UpperCase( *p );
//		p++;
//	}
////debug("converted: %s", s);
//
//	//return s
//    return s;
//}
//
////token_match
////parameter:
////	token:		the token to be matched
////	pattern:	the pattern to be matched
////	capitalize:	if TRUE then capitalize the token otherwise not
////return:
////	=TRUE	if	toekn.token==pattens
////  =FALSE	otherwise
////requires:
////	none
////modifies:
////	token->toekn may be capitalized
////errors:
////	none
//boolean
//token_match (token_t *token, char *pattern, boolean capitalize)
//{
//
//	//check the parameters
//	if (!token || token->type != TOK_STRING)
//		return FALSE;
//
//	//capitalize the token
//	if(capitalize)
//		to_uppercase(token->token);
//
////printf("match:(%s)-(%s)\n", token->token, pattern);
//	//compare the token string with the pattern
//	return (strcmp(token->token, pattern)==0);
//}
//
////token_match
////parameter:
////	token:		the string to be matched
////	pattern:	the pattern to be matched
////	capitalize:	if TRUE then capitalize the token otherwise not
////return:
////	=TRUE	if	toekn.token==pattens
////  =FALSE	otherwise
////requires:
////	none
////modifies:
////	string may be capitalized
////errors:
////	none
//boolean
//string_match (char *token, char *pattern, boolean capitalize)
//{
//
//	int rt;
//
//	//check the parameters
//	if (!token )
//		return FALSE;
//
//	//capitalize the token
//	if(capitalize)
//		to_uppercase(token);
//
//	//compare the token string with the pattern
//	rt =  (strcmp(token, pattern)==0);
//	return rt;
//}
//void token_dump(token_t *token)
//{
//
//	printf("[%d]:(%s)\n", token->type, token->token);	
//}
//int peek_token()
//{
//	int c;
//	c = getChar(PBPI_In);
//	ungetc( c, PBPI_In);
//	return c;
//}
//
//boolean read_token(char *pattern)
//{
//	do{
//	next_token( &currtok );
//	//token_dump(  &currtok );
//	}while( currtok.type == TOK_NEWLINE );
//	return token_match(  &currtok, pattern, TRUE );
//}
//
//void next_command()
//{
//      do{
//        next_token( &currtok );
//        //token_dump(  &currtok );
//        }while( currtok.type == TOK_NEWLINE );
//	if( currtok.type == TOK_STRING)
//	to_uppercase( currtok.token );
//}

