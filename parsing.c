#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include "mpc.h"

// lisp value (either a number or an error or a symbol or a symbolic expression)
typedef struct lval{
	int type;
	long num;
	char* err;
	char* sym;
	int count;
	struct lval** cell;
} lval;

// enum of possible lisp values
enum { LVAL_NUM, LVAL_ERR , LVAL_SYM, LVAL_SEXPR };

//constructors

lval* lval_num(long x) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_NUM;
	v->num = x;
	return v;
}

lval* lval_err(char* m) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_ERR;
	v->err = malloc(strlen(m) + 1);
	strcpy(v->err, m);
	return v;
}

lval* lval_sym(char* s) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_SYM;
	v->sym = malloc(strlen(s) + 1);
	strcpy(v->sym, s);
	return v;
}

lval* lval_sexpr(void) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_SEXPR;
	v->count = 0;
	v->cell = NULL;
	return v;
}

void lval_del(lval* v) {

	switch (v->type) {
		case LVAL_NUM: break;
		case LVAL_ERR: free(v->err); break;
		case LVAL_SYM: free(v->sym); break;
		case LVAL_SEXPR:
			for (int i = 0; i< v->count; i++) {
				lval_del(v->cell[i]);
			}
			free(v->cell);
		break;
	}
	free(v);
}

void lval_print(lval* v);

void lval_expr_print(lval* v, char open, char close) {
	putchar(open);
	for(int i = 0; i < v->count; i++) {
		lval_print(v->cell[i]);

		if(i != (v->count-1)) {
			putchar(' ');
		}
	}
	putchar(close);
}

void lval_print(lval* v) {
	switch (v->type) {
		case LVAL_NUM: printf("%ld", v->num); break;
		case LVAL_ERR: printf("Error: %s", v->err); break;
		case LVAL_SYM: printf("%s", v->sym); break;
		case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
	}
}

void lval_println(lval* v) { lval_print(v); putchar('\n'); }

lval* lval_read_num(mpc_ast_t* t) {
	errno = 0;
	long x = strtol(t->contents, NULL, 10);
	return errno != ERANGE ?
		lval_num(x) : lval_err("invalid number");
}

lval* lval_add(lval* parent, lval* child) {
	parent->count++;
	parent->cell = realloc(parent->cell, sizeof(lval*) * parent->count);
	parent->cell[parent->count-1] = child;
	return parent;
}

lval* lval_read(mpc_ast_t* t) {

	if(strstr(t->tag, "number")) { return lval_read_num(t); };
	if(strstr(t->tag, "symbol")) { return lval_sym(t->contents); };

	lval* x = NULL;
	if(strcmp(t->tag, ">") == 0) { x = lval_sexpr(); }
	if(strstr(t->tag, "sexpr"))  { x = lval_sexpr(); }
	
	for(int i = 0; i< t->children_num; i++) {
		if(strcmp(t->children[i]->contents, "(") == 0) { continue; }
		if(strcmp(t->children[i]->contents, ")") == 0) { continue; }
		if(strcmp(t->children[i]->tag, "regex") == 0)  { continue; }
		x = lval_add(x, lval_read(t->children[i]));
	}
	return x;
}

lval* lval_pop(lval* v, int i) {
	lval* result = v->cell[i];
	memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));
	v->count--;
	v->cell = realloc(v->cell, sizeof(lval*) * v->count);
	return result;
}

lval* lval_take(lval* v, int i) {
	lval* x = lval_pop(v, i);
	lval_del(v);
	return x;
}

lval* builtin_op(lval* a, char* op) {

	for (int i = 0; i < a->count; i++) {
		if (a->cell[i]->type != LVAL_NUM) {
			lval_del(a);
			return lval_err("Cannot operate on non number");
		}
	}
	
	lval* x = lval_pop(a, 0);

	if ((strcmp(op, "-") == 0) && a->count == 0) {
		x->num = -x->num;
	}

	while (a->count > 0) {
		lval* y = lval_pop(a, 0);

		if (strcmp(op, "+") == 0) { x->num += y->num; }		
		if (strcmp(op, "-") == 0) { x->num -= y->num; }
		if (strcmp(op, "*") == 0) { x->num *= y->num; }
		if (strcmp(op, "/") == 0) { 
			if (y->num == 0) {
				lval_del(x); lval_del(y);
				x = lval_err("Division by zero"); break;
			}
			x->num /= y->num;
		}
		lval_del(y);
	}
	lval_del(a);
	return x;
}


lval* lval_eval(lval* v);

lval* lval_eval_sexpr(lval* v) {

	for (int i = 0; i < v->count; i++) {
		v->cell[i] = lval_eval(v->cell[i]);
	}
	
	for(int i = 0; i < v->count; i++) {
		if(v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
	}

	if (v->count == 0) { return v; }

	if (v->count == 1) { return lval_take(v, 0); }

	lval* first = lval_pop(v, 0);
	if (first->type != LVAL_SYM) {
		lval_del(first); lval_del(v);
		return lval_err("S-expression does not begin with symbol!");
	}

	lval* result = builtin_op(v, first->sym);
	lval_del(first);
	return result;
}

lval* lval_eval(lval* v) {
	if(v->type == LVAL_SEXPR) { return lval_eval_sexpr(v); }
	return v;
}

int main(int argc, char** argv) {

	mpc_parser_t* Number = mpc_new("number");
	mpc_parser_t* Symbol = mpc_new("symbol");
	mpc_parser_t* Sexpr = mpc_new("sexpr");
	mpc_parser_t* Expr = mpc_new("expr");
	mpc_parser_t* Lispy= mpc_new("lispy");
	
	mpca_lang(MPCA_LANG_DEFAULT,
		"								\
			number	: /-?[0-9]+/ ;					\
			symbol  : '+' | '-' | '*' | '/' ;			\
			sexpr	: '(' <expr>* ')' ;				\
			expr	: <number> | <symbol> | <sexpr> ;		\
			lispy	: /^/ <expr>* /$/ ;			\
		",
		Number, Symbol, Sexpr, Expr, Lispy);
	
	puts("Lispy version 0.0.0.1");
	puts("Press Ctrl-C to exit\n");

	while(1) {
		char* input = readline("lispy> ");

		add_history(input);	

		mpc_result_t r;
		if (mpc_parse("<stdin>", input, Lispy, &r)) {
			lval* result = lval_eval(lval_read(r.output));
			lval_println(result);
			lval_del(result);
		} else {
			mpc_err_print(r.error);
			mpc_err_delete(r.error);
		}

		free(input);
	}

	mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Lispy);
	return 0;
}
