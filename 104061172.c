#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
/*
Something like Python
>> y = 2
>> z = 2
>> x = 3*y + 4/(2*z)
*/

/*
the only type: integer
everything is an expression
  statement   := END | expr END
  expr        := term expr_tail
  expr_tail   := ADDSUB term expr_tail | NIL
  term        := factor term_tail
  term_tail := MULDIV factor term_tail | NIL
  factor      := INT | ADDSUB INT | ADDSUB ID | ID ASSIGN expr | ID | LPAREN expr RPAREN
*/

#define MAXLEN 256

#define TBLSIZE 65535

typedef enum {UNKNOWN, END, INT, ID, ADDSUB, MULDIV, ASSIGN, LPAREN, RPAREN} TokenSet;

typedef struct {
	char name[MAXLEN];
	int val;
} Symbol;
Symbol table[TBLSIZE];
int sbcount = 0;

typedef struct _Node {
	char lexeme[MAXLEN];
	TokenSet token;
	int val;
	struct _Node *left, *right;
} BTNode;

void statement(void);
BTNode* expr(void);
BTNode* term(void);
BTNode* factor(void);
int getval(void);
int setval(char*, int);
void error(void);
int reg;
int have_ID[3];
bool err;
int linex;
int liney;
int linez;
/* Lexical-related function */
int match (TokenSet token);
void advance(void);
char* getLexeme(void);
static TokenSet getToken(void);
static TokenSet lookahead = UNKNOWN;
static char lexeme[MAXLEN];

/* create a node without any child */
BTNode* makeNode(TokenSet tok, const char *lexe)
{
	BTNode *node = (BTNode*) malloc(sizeof(BTNode));
	strcpy(node->lexeme, lexe);
	node->token= tok;
	node->val = 0;
	node->left = NULL;
	node->right = NULL;
	return node;
}

/* clean a tree */
void freeTree(BTNode *root)
{
	if (root!=NULL) {
		freeTree(root->left);
		freeTree(root->right);
		free(root);
	}
}

/* print a tree by pre-order */
void printPrefix(BTNode *root)
{
	if (root != NULL) {
		printf("%s ", root->lexeme);
		//printf("%d ^^",root->val);
		printPrefix(root->left);
		printPrefix(root->right);
	}
}

/* traverse the syntax tree by pre-order
   and evaluate the underlying expression */
int evaluateTree(BTNode *root)
{
	int retval = 0, lv, rv;
	if (root != NULL) {
		switch (root->token) {
		case ID:
            if(strcmp(root->lexeme,"x") == 0){
                printf("MOV r%d [0]\n",reg);
                reg++;
                if(linex == 0)
                    root->val = 13;
            }
            if(strcmp(root->lexeme,"y") == 0){
                printf("MOV r%d [4]\n",reg);
                reg++;
                if(liney == 0)
                    root->val = 13;
            }
            if(strcmp(root->lexeme,"z") == 0){
                printf("MOV r%d [8]\n",reg);
                reg++;
                if(linez == 0)
                    root->val = 13;
            }
            retval = root->val;
            break;
		case INT:
			retval = root->val;
            printf("MOV r%d %d\n",reg,root->val);
            reg++;
                /* TODO */
			break;
		case ASSIGN:
		case ADDSUB:
		case MULDIV:
                /* TODO */
                lv = evaluateTree(root->left);
                rv = evaluateTree(root->right);
                if (strcmp(root->lexeme, "+") == 0){
                    if(strcmp(root->right->lexeme,"=") == 0)
                    error();
                    printf("ADD r%d r%d\n",reg-2,reg-1);
                    reg--;
                    retval = lv + rv;
                }
                else if (strcmp(root->lexeme, "-") == 0){
                    if(strcmp(root->right->lexeme,"=") == 0)
                    error();
                    printf("SUB r%d r%d\n",reg-2,reg-1);
                    reg--;
                    retval = lv - rv;
                }
                else if (strcmp(root->lexeme, "*") == 0){
                    if(strcmp(root->right->lexeme,"=") == 0)
                    error();
                    printf("MUL r%d r%d\n",reg-2,reg-1);
                    reg--;
                    retval = lv * rv;
                }
                else if (strcmp(root->lexeme, "/") == 0){
                    if(strcmp(root->right->lexeme,"0") == 0)
                    error();
                    if(strcmp(root->right->lexeme,"=") == 0)
                    error();
                    //printf("rv = %d",root->right->val);
                    if(rv == 0)
                    error();
                    printf("DIV r%d r%d\n",reg-2,reg-1);
                    reg--;
                    retval = lv / rv;
                }
                 else if (strcmp(root->lexeme, "=") == 0){
                    if(strcmp(root->left->lexeme,"x") == 0){
                        printf("MOV [0] r%d\n",reg-1);
                        have_ID[0] = 1;
                        retval = setval(root->left->lexeme, rv);
                    }
                    if(strcmp(root->left->lexeme,"y") == 0){
                        printf("MOV [4] r%d\n",reg-1);
                        have_ID[1] = 1;
                        retval = setval(root->left->lexeme, rv);
                    }
                    if(strcmp(root->left->lexeme,"z") == 0){
                        printf("MOV [8] r%d\n",reg-1);
                        have_ID[2] = 1;
                        retval = setval(root->left->lexeme, rv);
                    }
                    retval = setval(root->left->lexeme, rv);
                 }
                break;
		default:
			retval = 0;
		}
	}
	return retval;
}

int getval(void)
{
	int i, found, retval = 0;

	if (match(INT)) {
		retval = atoi(getLexeme());
	} else if (match(ID)) {
		i = 0;
		found = 0;
		retval = 0;
		while (i<sbcount && !found) {
			if (strcmp(getLexeme(), table[i].name)==0) {
				retval = table[i].val;
				found = 1;
				break;
			} else {
				i++;
			}
		}
		if (!found) {
			if (sbcount < TBLSIZE) {
				strcpy(table[sbcount].name, getLexeme());
				table[sbcount].val = 0;
				sbcount++;
			} else {
				error();
			}
		}
	}
	return retval;
}

int setval(char *str, int val)
{
	int i, retval = 0;
	i = 0;
	while (i<sbcount) {
		if (strcmp(str, table[i].name)==0) {
			table[i].val = val;
			retval = val;
			break;
		} else {
			i++;
		}
	}
	return retval;
}

//  expr        := term expr_tail
//  expr_tail   := ADDSUB term expr_tail | NIL
BTNode* expr(void)
{
	BTNode *retp, *left;
	retp = left = term();
	while (match(ADDSUB)) { // tail recursion => while
		retp = makeNode(ADDSUB, getLexeme());
		advance();
		retp->right = term();
		retp->left = left;
		left = retp;
	}
	return retp;
}

//  term        := factor term_tail
//  term_tail := MULDIV factor term_tail | NIL
BTNode* term(void)
{
	BTNode *retp, *left;
	retp = left = factor();
	while (match(MULDIV)) { // tail recursion => while
		retp = makeNode(MULDIV, getLexeme());
		advance();
		retp->right = factor();
		retp->left = left;
		left = retp;
	}
	return retp;
}

BTNode* factor(void)
{
	BTNode* retp = NULL;
	char tmpstr[MAXLEN];

	if (match(INT)) {
		retp =  makeNode(INT, getLexeme());
		retp->val = getval();
		advance();
	} else if (match(ID)) {
		BTNode* left = makeNode(ID, getLexeme());
		left->val = getval();

		strcpy(tmpstr, getLexeme());
		advance();
		if (match(ASSIGN)) {
			retp = makeNode(ASSIGN, getLexeme());
			advance();
			retp->right = expr();
			retp->left = left;
		}
		else if(match(ID) || match(INT)){// ID 後 不能是 ID || INT
            error();
		}
		else {
			retp = left;
		}
	} else if (match(ADDSUB)) {
		strcpy(tmpstr, getLexeme());
		advance();
		if (match(ID) || match(INT)) {
			retp = makeNode(ADDSUB, tmpstr);
			if (match(ID))
				retp->right = makeNode(ID, getLexeme());
			else
				retp->right = makeNode(INT, getLexeme());
			retp->right->val = getval();
			retp->left = makeNode(INT, "0");
			retp->left->val = 0;
			advance();
		}
		else{
            error();
		}
	} else if (match(LPAREN)) {
		advance();
		retp = expr();
		if (match(RPAREN)) {
			advance();
		}
		else error();
	}
	else
        error();
	return retp;
}

void error(void)
{
    /* TODO:
     *
     * Error-Handler,
     * You should deal with the error that happened in calculator
     * An example is x = 5 / 0, which is divide zero error.
     * You should call error() when any error occurs
     *
     */
    err = true;
    printf("EXIT 1\n");
    exit(0);
}
////////EDIT
int top;
void push();
void pop();
BTNode* stack[500];

bool isoperator(BTNode*);
bool isoperator(BTNode* entry){
    if(strcmp(entry->lexeme,"+") == 0)
        return true;
    else if(strcmp(entry->lexeme,"-") == 0)
        return true;
    else if(strcmp(entry->lexeme,"*") == 0)
        return true;
    else if(strcmp(entry->lexeme,"/") == 0)
        return true;
    else
        return false;
}

bool isnumber(BTNode*);
bool isnumber(BTNode* entry){
if(entry->token == INT)
    return true;
else
    return false;
}

bool divideequal(BTNode*);
bool divideequal(BTNode* target){
if(  (strcmp(target->left->lexeme,"x") == 0)  && (strcmp(target->right->lexeme,"x") == 0)   )
return true;
if(  (strcmp(target->left->lexeme,"y") == 0)  && (strcmp(target->right->lexeme,"y") == 0)   )
return true;
if(  (strcmp(target->left->lexeme,"y") == 0)  && (strcmp(target->right->lexeme,"y") == 0)   )
return true;

return false;
}

int haseq = 0;

void push(BTNode*);
void push(BTNode* input){
stack[top++] = input;
return;
}

bool isID(BTNode* root);


void stackevaluatree(BTNode* root);
void stackevaluatree(BTNode* root){
    if(root){
            if(strcmp(root->lexeme,"=") == 0)
            haseq++;
            else if(isoperator(root)){
                if(isnumber(root->left) && isnumber(root->right))
                push(root);
                //printf("%d\n",top);
                /*if(strcmp(root->lexeme,"/") == 0){
                    //printf("root->right->val =%d",root->right->val);
                    //if(root->left->val == 0 && root->right->val == 0) // 0/0 undefine it can be anything
                    //error();
                   if(divideequal(root)){
                    root->val = 1;
                    root->lexeme[0] = '1';
                    root->lexeme[1] = '\0';
                    root->token = INT;
                    BTNode* frl = root->left;
                    BTNode* frr = root->right;
                    root->left = root->right = NULL;
                    free(frl);
                    free(frr);
                   }
                }*/
                if(strcmp(root->lexeme,"-") == 0){
                    if(divideequal(root)){
                        root->val = 0;
                        root->lexeme[0] = '0';
                        root->lexeme[1] = '\0';
                        root->token = INT;
                        BTNode* frl = root->left;
                        BTNode* frr =root->right;
                        root->left = root->right = NULL;
                        free(frl);
                        free(frr);
                    }
                }
            }
            stackevaluatree(root->left);
            stackevaluatree(root->right);
    }
}

void popcal(){
    top--;
    if(strcmp(stack[top]->lexeme,"+") == 0){
        stack[top]->val = stack[top]->left->val + stack[top]->right->val;
    }
    if(strcmp(stack[top]->lexeme,"-") == 0){
        stack[top]->val = stack[top]->left->val - stack[top]->right->val;
    }
    if(strcmp(stack[top]->lexeme,"*") == 0){
        stack[top]->val = stack[top]->left->val * stack[top]->right->val;
    }
    if(strcmp(stack[top]->lexeme,"/") == 0){
        if(stack[top]->right->val == 0)
        error();
        stack[top]->val = stack[top]->left->val / stack[top]->right->val;
    }
        sprintf(stack[top]->lexeme,"%d",stack[top]->val);
        stack[top]->token = INT;
        BTNode* frl = stack[top]->left;
        BTNode* frr = stack[top]->right;
        stack[top]->left = stack[top]->right = NULL;
        free(frl);
        free(frr);
}

void check(BTNode**);
void check(BTNode** root){
    while(1){
        haseq = 0;
        stackevaluatree(*root);
        //printf("%d",haseq);
        if(haseq > 1)
        error();
        if(haseq && top == 0)
            break;
        while(top != 0){
        popcal();
        }
        //printPrefix(*root);
        if(haseq > 1)
        error();
    }
}



////////
void statement(void)
{
	BTNode* retp;

	if (match(END)) {
		advance();
	} else {
		retp = expr();//construct Tree Here
		if (match(END)) {
            haseq = 0;
            check(&retp);
            evaluateTree(retp);
			//printPrefix(retp);
			//printf("\n");
			int i;
			reg = 0;
			if(have_ID[0] == 1)
                linex++;
            if(have_ID[1] == 1)
                liney++;
            if(have_ID[2] == 1)
                linez++;
            for(i = 0; i<3;i++){
                if(have_ID[i] == 1){
                    printf("MOV r%d [%d]\n",i,i*4);
                }
                    have_ID[i] = 0;
            }
			freeTree(retp);
			advance();
		}
	}
}

TokenSet getToken(void)
{
    int i;
    char c;

    while ( (c = fgetc(stdin)) == ' ' || c== '\t' );

    if (isdigit(c)) {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isdigit(c) && i<MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return INT;
    } else if (c == '+' || c == '-') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return ADDSUB;
    } else if (c == '*' || c == '/') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return MULDIV;
    } else if (c == '\n') {
        lexeme[0] = '\0';
        return END;
    } else if (c == '=') {
        strcpy(lexeme, "=");
        return ASSIGN;
    } else if (c == '(') {
        strcpy(lexeme, "(");
        return LPAREN;
    } else if (c == ')') {
        strcpy(lexeme, ")");
        return RPAREN;
    } /*else if (isalpha(c) || c == '_') {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isalpha(c) || isdigit(c) || c == '_') {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return ID;
    } */
    else if(c == 'x' || c == 'y' || c == 'z'){
        lexeme[0] = c;
        lexeme[1] ='\0';
        return ID;
    }
    else if(c == EOF){
    if(err == 0)
	{
        printf("MOV r0, [0]\n");
        printf("MOV r1, [4]\n");
        printf("MOV r2, [8]\n");
        printf("EXIT 0\n");
	}
	exit(0);
    }
    else {
        return UNKNOWN;
    }
}

void advance(void)
{
    lookahead = getToken();
}

int match(TokenSet token)
{
    if (lookahead == UNKNOWN) advance();
    return token == lookahead;
}

char* getLexeme(void)
{
    return lexeme;
}

int main()
{
    char c;

	while (1) {
		statement();
	}
	return 0;
}
