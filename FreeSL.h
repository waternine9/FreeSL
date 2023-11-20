#ifndef FREESL_H
#define FREESL_H

#ifdef FREESL_FREESTANDING

#ifdef FREESL_MALLOC_IMPL
#define malloc(_Size) FREESL_MALLOC_IMPL(_Size)
#else
#error "freesl: Please provide a `malloc` implementation defined as FREESL_MALLOC_IMPL"
#endif

#ifdef FREESL_FREE_IMPL
#define free(_Block) FREESL_FREE_IMPL(_Block)
#else
#error "freesl: Please provide a `free` implementation defined as FREESL_FREE_IMPL"
#endif



#else

#include <stdlib.h>

#endif // FREESL_FREESTANDING

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#ifndef FREESL_ERR
#error "freesl: Please provide a `FREESL_ERR` macro that takes in: \
		an error message C string as first parameter, \
		character where error happened as second parameter, \
		line where error happened as third parameter"
#endif

typedef enum 
{
	FSL_TOK_ASSIGN,
	FSL_TOK_ADD,
	FSL_TOK_SUB,
	FSL_TOK_MUL,
	FSL_TOK_DIV,
	FSL_TOK_ADDASSIGN,
	FSL_TOK_SUBASSIGN,
	FSL_TOK_MULASSIGN,
	FSL_TOK_DIVASSIGN,
	FSL_TOK_VARASSIGN,
	FSL_TOK_VAR,
	FSL_TOK_CONST,
	FSL_TOK_CONSTRUCTOR,
	FSL_TOK_SWIZZLE,
	FSL_TOK_CAST,
	FSL_TOK_CALL,
	FSL_TOK_GET,
	FSL_TOK_IF,
	FSL_TOK_WHILE,
	FSL_TOK_SHL,
	FSL_TOK_SHR,
	FSL_TOK_PREINC,
	FSL_TOK_PREDEC,
	FSL_TOK_POSTINC,
	FSL_TOK_POSTDEC,
	FSL_TOK_EQUALS,
	FSL_TOK_NOTEQ,
	FSL_TOK_OR,
	FSL_TOK_NOT,
	FSL_TOK_AND,
	FSL_TOK_TYPE,
	FSL_TOK_INVALID,
	FSL_TOK_RETURN
} FreeSL_TokenType;

typedef enum 
{
	FSL_TYPE_VOID,
	FSL_TYPE_INT,
	FSL_TYPE_FLOAT,
	FSL_TYPE_VEC2,
	FSL_TYPE_VEC3,
	FSL_TYPE_VEC4,
	FSL_TYPE_MAT2,
	FSL_TYPE_MAT3,
	FSL_TYPE_MAT4,
	FSL_TYPE_CUSTOM
} FreeSL_TypeOrigin;

typedef struct
{
	char* data;
	int size;
} FreeSL_String;

typedef struct _FreeSL_Type
{
	FreeSL_String name;
	FreeSL_TypeOrigin origin;
	int size;

	// Only if origin is built-in
	bool floating_point;
	int count;

	// Only if origin is custom
	struct _FreeSL_Type* members;
	int member_count;
} FreeSL_Type;


typedef struct
{
	FreeSL_String name;
	FreeSL_Type* type;
	void* data;
	bool is_array;

	// Only if is array
	int count;
} FreeSL_Var;

typedef struct
{
	FreeSL_TypeOrigin type; // Not FreeSL_Type because constants can only be builtin types
	void* data;
} FreeSL_Const;

typedef struct _FreeSL_Scope
{
	FreeSL_Var** vars; // Local variables to this scope
	size_t vars_count;

	struct _FreeSL_Scope* parent;
} FreeSL_Scope;

struct _FreeSL_Token;

typedef struct
{
	FreeSL_String name;

	struct _FreeSL_Token** lines;
	int line_count;

	FreeSL_Scope* scope;
	size_t param_count;

	FreeSL_Type* return_type;
} FreeSL_Function;

typedef struct _FreeSL_Token
{
	FreeSL_TokenType type;
	
	// Only if is var
	FreeSL_Var* var;

	// Only if is const
	FreeSL_Const constant;

	// Only if is operator
	struct _FreeSL_Token* lhs;
	struct _FreeSL_Token* rhs;

	// Only if is prefix/postfix operator
	struct _FreeSL_Token* expr;

	// Only if is function call
	FreeSL_Function* function;
	struct _FreeSL_Token** args;

	// Only if is constructor or cast
	FreeSL_Function* target_type;
} FreeSL_Token;

typedef struct
{
	bool is_function;
	
	// If is function decl
	FreeSL_Function* funcdecl;

	// If is var decl
	FreeSL_Token* vardecl;
} FreeSL_RootLine;

typedef struct
{
	FreeSL_Scope root_scope;
	FreeSL_RootLine* roots;
	size_t roots_count;
} FreeSL_AST;

typedef struct
{
	char* code;
	char* code_base;
	FreeSL_AST ast;

	int cur_line;
	int cur_char;

	FreeSL_Type* types;
	int types_size;
} FreeSL_Tokenizer;

// `code` must be null terminated
FreeSL_AST FreeSL_GenAST(char* code);

#endif // FREESL_H