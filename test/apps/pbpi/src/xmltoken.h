#ifndef _PBPI_XML_TOKEN_H
#define _PBPI_XML_TOKEN_H

#define	MAX_XML_TOKEN			63
#define	MAX_TOKEN_STACK_SIZE	16
#define MAX_TOKEN_PATH_LEN		1023
typedef enum XMLTokenType {XML_KEY_BEGIN, XML_VALUE, XML_KEY_END, XML_ERROR} XMLTokenType;

typedef struct XMLToken
{
	XMLTokenType		type;
	char	token[MAX_XML_TOKEN + 1];
} XMLToken;

typedef struct XMLTokenSpace
{
	XMLToken *	list;
	int			size;
} XMLTokenSpace;

int nextXMLToken(FILE *in, XMLToken *xmltoken);
void dumpXMLToken(XMLToken *xmltoken);

int tokenStackPush(XMLToken *xmltoken);
int tokenStackPop(XMLToken *xmltoken);
void tokenStackPeek(XMLToken *xmltoken);

void getTokenSpace(XMLTokenSpace *tokenSpace);
void dumpTokenSpace(XMLTokenSpace *tokenSpace);
char *tokenSpace2String(XMLTokenSpace *tokenSpace);

#endif //_PBPI_XML_TOKEN_H

