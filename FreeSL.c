#define _CRT_SECURE_NO_WARNINGS
#include <stdlib.h>
void errmsg(const char* msg, int ch, int line)
{
	printf("ERROR: %s\nLine: %i\nCharacter: %i", msg, ch, line);
	abort();
}
#define FREESL_ERR(msg, ch, line) errmsg(msg, ch, line);

#include "FreeSL.h"

#include <stdio.h>


#define min(x, y) ((x) < (y) ? (x) : (y))
#define max(x, y) ((x) > (y) ? (x) : (y))

#define OP_ADD "+"
#define OP_SUB "-"
#define OP_MUL "*"
#define OP_DIV "/"
#define OP_ASSIGN "="
#define OP_GET "."
#define OP_LPA "("
#define OP_RPA ")"
#define OP_DEC "--"
#define OP_INC "++"
#define OP_ADDASSIGN "+="
#define OP_SUBASSIGN "-="
#define OP_MULASSIGN "*="
#define OP_DIVASSIGN "/="
#define OP_EQUALS "=="
#define OP_NOTEQ "!="
#define OP_AND "&&"
#define OP_OR "||"
#define OP_NOT "!"
#define OP_SHL "<<"
#define OP_SHR ">>"
#define OP_RETURN "return"

#define LINE_DELIMITER ';'

static char op_chars[] = { '+', '-', '*', '/', '.', '&', '=', '(', ')' };

typedef struct
{
	const char* op_str;
	FreeSL_TokenType op_tok;
} FreeSL_OpTokPair;

typedef struct
{
	FreeSL_TokenType op;
	int precedence; // operations are executed in order from lowest to highest precedence
} FreeSL_OpPrecedence;

typedef struct
{
	bool is_op;

	FreeSL_Token* expr;
	FreeSL_TokenType op;
} FreeSL_ExprOrOp;

static FreeSL_OpTokPair op_map[] = {
	{ OP_ADD,		FSL_TOK_ADD },
	{ OP_SUB,		FSL_TOK_SUB },
	{ OP_MUL,		FSL_TOK_MUL },
	{ OP_DIV,		FSL_TOK_DIV },
	{ OP_ASSIGN,	FSL_TOK_ASSIGN },
	{ OP_GET,		FSL_TOK_GET },
	{ OP_ADDASSIGN, FSL_TOK_ADDASSIGN },
	{ OP_SUBASSIGN, FSL_TOK_SUBASSIGN },
	{ OP_MULASSIGN, FSL_TOK_MULASSIGN },
	{ OP_DIVASSIGN, FSL_TOK_DIVASSIGN },
	{ OP_EQUALS,	FSL_TOK_EQUALS },
	{ OP_NOTEQ,		FSL_TOK_NOTEQ },
	{ OP_OR,		FSL_TOK_OR },
	{ OP_SHL,		FSL_TOK_SHL },
	{ OP_SHR,		FSL_TOK_SHR },
	{ OP_AND,		FSL_TOK_AND },
	{ OP_ADDASSIGN,	FSL_TOK_ADDASSIGN },
	{ OP_SUBASSIGN,	FSL_TOK_SUBASSIGN },
	{ OP_MULASSIGN,	FSL_TOK_MULASSIGN },
	{ OP_DIVASSIGN,	FSL_TOK_DIVASSIGN },
	{ OP_NOT,		FSL_TOK_NOT },
	{ OP_DEC,		FSL_TOK_PREDEC },
	{ OP_INC,		FSL_TOK_PREINC },
	{ OP_DEC,		FSL_TOK_POSTDEC },
	{ OP_INC,		FSL_TOK_POSTINC },
	{ OP_RETURN,	FSL_TOK_RETURN }
};

static FreeSL_OpTokPair op_map_middle[] = {
	{ OP_ADD,		FSL_TOK_ADD },
	{ OP_SUB,		FSL_TOK_SUB },
	{ OP_MUL,		FSL_TOK_MUL },
	{ OP_DIV,		FSL_TOK_DIV },
	{ OP_ASSIGN,	FSL_TOK_ASSIGN },
	{ OP_GET,		FSL_TOK_GET },
	{ OP_ADDASSIGN, FSL_TOK_ADDASSIGN },
	{ OP_SUBASSIGN, FSL_TOK_SUBASSIGN },
	{ OP_MULASSIGN, FSL_TOK_MULASSIGN },
	{ OP_DIVASSIGN, FSL_TOK_DIVASSIGN },
	{ OP_EQUALS,	FSL_TOK_EQUALS },
	{ OP_NOTEQ,		FSL_TOK_NOTEQ },
	{ OP_OR,		FSL_TOK_OR },
	{ OP_SHL,		FSL_TOK_SHL },
	{ OP_SHR,		FSL_TOK_SHR },
	{ OP_RETURN,	FSL_TOK_RETURN }
};

static FreeSL_OpTokPair op_map_prefix[] = {
	{ OP_NOT,		FSL_TOK_NOT },
	{ OP_DEC,		FSL_TOK_PREDEC },
	{ OP_INC,		FSL_TOK_PREINC },
};

static FreeSL_OpTokPair op_map_postfix[] = {
	{ OP_DEC,		FSL_TOK_POSTDEC },
	{ OP_INC,		FSL_TOK_POSTINC },
};

static bool IsOpPrefix(FreeSL_TokenType op)
{
	for (int i = 0; i < sizeof(op_map_prefix) / sizeof(FreeSL_OpTokPair); i++)
	{
		if (op_map_prefix[i].op_tok == op) return true;
	}
	return false;
}

static bool IsOpPostfix(FreeSL_TokenType op)
{
	for (int i = 0; i < sizeof(op_map_postfix) / sizeof(FreeSL_OpTokPair); i++)
	{
		if (op_map_postfix[i].op_tok == op) return true;
	}
	return false;
}

static bool IsOpMiddle(FreeSL_TokenType op)
{
	for (int i = 0; i < sizeof(op_map_middle) / sizeof(FreeSL_OpTokPair); i++)
	{
		if (op_map_middle[i].op_tok == op) return true;
	}
	return false;
}

static FreeSL_OpPrecedence op_precedence[] = {
	{ FSL_TOK_POSTINC,		0 },
	{ FSL_TOK_POSTDEC,		0 },
	////////////////////////////
	{ FSL_TOK_PREINC,		1 },
	{ FSL_TOK_PREDEC,		1 },
	{ FSL_TOK_CAST,			1 },
	{ FSL_TOK_NOT,			1 },
	////////////////////////////
	{ FSL_TOK_DIV,			2 },
	{ FSL_TOK_MUL,			2 },
	////////////////////////////
	{ FSL_TOK_ADD,			3 },
	{ FSL_TOK_SUB,			3 },
	////////////////////////////
	{ FSL_TOK_EQUALS,		3 },
	{ FSL_TOK_NOTEQ,		3 },
	////////////////////////////
	{ FSL_TOK_AND,			4 },
	////////////////////////////
	{ FSL_TOK_OR,			5 },
	////////////////////////////
	{ FSL_TOK_ASSIGN,		6 },
	{ FSL_TOK_ADDASSIGN,	6 },
	{ FSL_TOK_SUBASSIGN,	6 },
	{ FSL_TOK_MULASSIGN,	6 },
	{ FSL_TOK_DIVASSIGN,	6 },
	///////////////////////////,
	{ FSL_TOK_RETURN,       7 }

};

#define MAX_PRECEDENCE 7

static int GetPrecedence(FreeSL_TokenType op)
{
	for (int i = 0; i < (sizeof(op_precedence) / sizeof(FreeSL_OpPrecedence)); i++)
	{
		if (op_precedence[i].op == op) return op_precedence[i].precedence;
	}
	return -1;
}

static void CopyData(void* dest, void* src, int size)
{
	for (int i = 0; i < size;i++) ((uint8_t*)dest)[i] = ((uint8_t*)src)[i];
}

static void* ResizeData(void* data, size_t old_size, size_t new_size)
{ 
	void* new_data = malloc(new_size);
	CopyData(new_data, data, min(old_size, new_size));
	free(data);
	return new_data;
}

static void* ResizeDataNoFree(void* data, size_t old_size, size_t new_size)
{
	void* new_data = malloc(new_size);
	CopyData(new_data, data, min(old_size, new_size));
	return new_data;
}

static bool IsDigit(char c)
{
	return c >= '0' && c <= '9';
}

static bool IsNumberChar(char c)
{
	return IsDigit(c) || c == '.' || c == 'f' || c == '-';
}

static bool IsSymbolChar(char c)
{
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || IsDigit(c);
}

static bool IsWhitespaceChar(char c)
{
	return c == ' ' || c == '\n' || c == '\t';
}

static int StringLength(const char* str)
{
	int len = 0;
	while (str[len++]);
	return len;
}

static bool StringCmpWithLen(const char* str_a, const char* str_b, int len)
{
	while (len--) if (str_a[len] != str_b[len]) return false;
	return true;
}

static bool StringCmp(FreeSL_String str_a, FreeSL_String str_b)
{
	if (str_a.size != str_b.size) return false;
	while (str_a.size--) if (str_a.data[str_a.size] != str_b.data[str_a.size]) return false;
	return true;
}

static void Advance(FreeSL_Tokenizer* t, int num)
{
	while (num--)
	{
		if (t->code[0] == '\n')
		{
			t->cur_char = 0;
			t->cur_line++;
		}
		else
		{
			t->cur_char++;
		}
		t->code++;
	}
}

static int FindNextChar(FreeSL_Tokenizer* t, char c)
{
	for (int i = 0; t->code[i]; i++)
	{
		if (t->code[i] == c) return i;
	}
	return -1;
}

static char TellNextChar(FreeSL_Tokenizer* t, int offset)
{
	for (int i = 0; i < offset; i++)
	{
		if (!t->code[i]) return -1;
	}

	for (int i = 0; t->code[i + offset]; i++)
	{
		if (!IsWhitespaceChar(t->code[i + offset])) return t->code[i + offset];
	}
	return -1;
}

// assumes that tokenizer is already at or behind the desired initial increment char
static int FindNextMatchingChar(FreeSL_Tokenizer* t, char c_inc, char c_dec)
{
	int nest_count = 0;
	for (int i = 0; t->code[i]; i++)
	{
		if (t->code[i] == c_inc) nest_count++;
		if (t->code[i] == c_dec) nest_count--;
		if (t->code[i] == c_dec && nest_count == 0) return i;
	}
	return -1;
}

// same as FindNextMatchingChar except also stops searching at a specified character
static int FindNextMatchingCharStop(FreeSL_Tokenizer* t, char c_inc, char c_dec, char c_stop)
{
	int nest_count = 0;
	for (int i = 0; t->code[i]; i++)
	{
		if (t->code[i] == c_inc) nest_count++;
		if (t->code[i] == c_dec) nest_count--;
		if (t->code[i] == c_stop && nest_count == 0) return -1;
		if (t->code[i] == c_dec && nest_count == 0) return i;
	}
	return -1;
}

// assumes that tokenizer is already at or behind the desired initial increment char
static int FindScopedChar(FreeSL_Tokenizer* t, char c_inc, char c_dec, char c_target)
{
	int nest_count = 0;
	for (int i = 0; t->code[i]; i++)
	{
		if (t->code[i] == c_inc) nest_count++;
		if (t->code[i] == c_dec) nest_count--;
		if (t->code[i] == c_target && nest_count == 0) return i;
	}
	return -1;
}

// assumes that tokenizer is already at or behind the desired initial increment char
static int FindScopedCharStop(FreeSL_Tokenizer* t, char c_inc, char c_dec, char c_target, char c_stop)
{
	int nest_count = 0;
	for (int i = 0; t->code[i]; i++)
	{
		if (t->code[i] == c_inc) nest_count++;
		if (t->code[i] == c_dec) nest_count--;
		if (t->code[i] == c_stop && nest_count == 0) return -1;
		if (t->code[i] == c_target && nest_count == 0) return i;
	}
	return -1;
}

// almost same as FindMatchingChar except returns size when reached end of code too
static int GetSymbolSize(FreeSL_Tokenizer* t)
{
	int i = 0;
	for (; t->code[i]; i++)
	{
		if (!IsSymbolChar(t->code[i])) return i;
	}
	return i;
}

static bool IsOpChar(char c)
{
	for (int i = 0; i < (sizeof op_chars); i++)
	{
		if (op_chars[i] == c) return true;
	}
	return false;
}

static int GetOpLen(FreeSL_Tokenizer* t)
{
	int i = 0;
	for (; t->code[i]; i++)
	{
		if (!IsOpChar(t->code[i])) break;
	}
	return i;
}

static FreeSL_TokenType IdentifyOp(FreeSL_Tokenizer* t)
{
	int len = GetOpLen(t);

	for (int i = 0; i < sizeof(op_map) / sizeof(FreeSL_OpTokPair); i++)
	{
		const char* op_str = op_map[i].op_str;
		if (StringLength(op_str) != len) continue;
		if (StringCmpWithLen(op_str, t->code, len)) return op_map[i].op_tok;
	}

	return FSL_TOK_INVALID;
}

static void SkipWhitespace(FreeSL_Tokenizer* t)
{
	while (t->code[0])
	{
		if (!IsWhitespaceChar(t->code[0])) return;
		Advance(t, 1);
	}
}

static void SkipWhitespaceSafe(FreeSL_Tokenizer* t)
{
	SkipWhitespace(t);
	if (!t->code[0]) FREESL_ERR("Unexpected end of code", t->cur_char, t->cur_line);
}

static FreeSL_Type* FindType(FreeSL_Tokenizer* t, FreeSL_String name)
{
	for (int i = 0; i < t->types_size; i++)
	{
		if (StringCmp(t->types[i].name, name)) return &t->types[i];
	}
	return 0;
}

static FreeSL_Function* FindFunction(FreeSL_Tokenizer* t, FreeSL_String name)
{
	for (int i = 0; i < t->ast.roots_count; i++)
	{
		if (!t->ast.roots[i].is_function) continue;
		if (StringCmp(t->ast.roots[i].funcdecl->name, name)) return t->ast.roots[i].funcdecl;
	}
	return 0;
}

static FreeSL_Var* FindVarInScope(FreeSL_Scope* scope, FreeSL_String name)
{
	for (int i = 0; i < scope->vars_count; i++)
	{
		if (StringCmp(scope->vars[i]->name, name)) return &scope->vars[i];
	}
	
	if (scope->parent) return FindVarInScope(scope->parent, name);
	return 0;
}

// When calling, make sure there is no beginning whitespace
// When this is finished, `t->code` will ALWAYS be at `stop_at`
static FreeSL_Token* TokenizeExpr(FreeSL_Tokenizer* t, FreeSL_Scope* scope, char* stop_at);

// When this is finished, advances to the first character after the number
static FreeSL_Token* TokenizeNumber(FreeSL_Tokenizer* t)
{
	bool negative = false;

	bool is_float = false;
	int decimal = 0;

	int step = 0;
	
	if (t->code[0] == '-')
	{
		negative = true;
		step++;
	}

	while (t->code[step])
	{
		if (IsWhitespaceChar(t->code[step])) break;
		if (t->code[step] == 'f')
		{
			if (!is_float) FREESL_ERR("Unexpected character in number", t->cur_char + step, t->cur_line);
			break;
		}
		if (!IsNumberChar(t->code[step])) FREESL_ERR("Unexpected character in number", t->cur_char + step, t->cur_line);
		if (t->code[step] == '-') FREESL_ERR("Unexpected character in number", t->cur_char + step, t->cur_line);
		if (t->code[step] == '.')
		{
			if (is_float) FREESL_ERR("Unexpected character in number", t->cur_char + step, t->cur_line);
			decimal = step;
			is_float = true;
		}
		step++;
	}

	if (!t->code[step]) FREESL_ERR("Unexpected end of code while parsing number", t->cur_char + step, t->cur_line);

	FreeSL_Token* tok = (FreeSL_Token*)malloc(sizeof(FreeSL_Token));
	tok->type = FSL_TOK_CONST;

	if (is_float)
	{
		float num = 0.0f;
		float mult = 1.0f;
		for (int i = decimal - 1; i >= (negative ? 1 : 0); i--)
		{
			num += (float)(t->code[i] - '0') * mult;
			mult *= 10;
		}
		mult = 0.1f;
		for (int i = decimal + 1; i <= (t->code[step] == 'f' ? step - 1 : step); i++)
		{
			num += (float)(t->code[i] - '0') * mult;
			mult /= 10;
		}
		if (negative) num *= -1;

		tok->constant.type = FSL_TYPE_FLOAT;
		tok->constant.data = malloc(sizeof(float));
		*(float*)tok->constant.data = num;
	}
	else
	{
		int num = 0;
		int mult = 1;
		for (int i = step; i >= (negative ? 1 : 0); i--)
		{
			num += (int)(t->code[i] - '0') * mult;
			mult *= 10;
		}
		if (negative) num *= -1;

		tok->constant.type = FSL_TYPE_INT;
		tok->constant.data = malloc(sizeof(int));
		*(int*)tok->constant.data = num;
	}
	
	Advance(t, step + 1);

	return tok;
}

// When this is finished, advances to the first character after the variable name
static FreeSL_Var* TokenizeVarDecl(FreeSL_Tokenizer* t, FreeSL_Scope* scope)
{
	int len = GetSymbolSize(t);
	FreeSL_String symbol;
	symbol.data = t->code;
	symbol.size = len;

	FreeSL_Type* type = FindType(t, symbol);
	if (!type) FREESL_ERR("Unrecognized type in variable declaration", t->cur_char, t->cur_line);

	Advance(t, len);
	SkipWhitespace(t);

	if (IsSymbolChar(t->code[0]))
	{
		len = GetSymbolSize(t);
		FreeSL_Var* new_var = malloc(sizeof(FreeSL_Var));
		new_var->type = type;
		new_var->name.data = ResizeDataNoFree(t->code, len, len);
		new_var->name.size = len;

		return new_var;
	}
	else
	{
		FREESL_ERR("Expected variable name with type", t->cur_char, t->cur_line);
	}
	return 0;
}

// When calling, start at the starting parantheses
// When this is finished, advances to the closing parantheses
static void TokenizeArgs(FreeSL_Tokenizer* t, FreeSL_Scope* scope, FreeSL_Token* enclosing, int target_arguments)
{
	int args_count = 0;

	while (true)
	{
		Advance(t, 1);

		SkipWhitespace(t);

		int next_comma = FindScopedCharStop(t, '(', ')', ',', ';');
		int end_loc = FindScopedCharStop(t, '(', ')', ')', ';');

		int stop_at = min(next_comma, end_loc);
		if (stop_at == 0) break;
		if (stop_at == -1) FREESL_ERR("Expected new argument or closing parantheses", t->cur_char, t->cur_line);


		enclosing->args[args_count++] = TokenizeExpr(t, scope, t->code + stop_at);
		enclosing->args = ResizeData(enclosing->args, args_count * sizeof(FreeSL_Token*), (args_count + 1) * sizeof(FreeSL_Token*));

		if (t->code[0] == ')') break;
	}

	if (args_count != target_arguments) FREESL_ERR("Invalid number of arguments", t->cur_char, t->cur_line);
}

// When calling, start at the starting parantheses
// When this is finished, advances to the closing parantheses
static void TokenizeParams(FreeSL_Tokenizer* t, FreeSL_Scope* scope, FreeSL_Function* enclosing)
{
	while (true)
	{
		Advance(t, 1);

		SkipWhitespace(t);

		int next_comma = FindScopedCharStop(t, '(', ')', ',', ';');
		int end_loc = FindScopedCharStop(t, '(', ')', ')', ';');

		int stop_at = min(next_comma, end_loc);
		if (stop_at == 0) break;
		if (stop_at == -1) FREESL_ERR("Expected new argument or closing parantheses", t->cur_char, t->cur_line);


		enclosing->scope->vars[enclosing->param_count++] = TokenizeVarDecl(t, scope, t->code + stop_at);
		enclosing->scope->vars = ResizeData(enclosing->scope->vars, enclosing->param_count * sizeof(FreeSL_Token*), (enclosing->param_count + 1) * sizeof(FreeSL_Token*));

		if (t->code[0] == ')') break;
	}

	enclosing->scope->vars_count = enclosing->param_count;
}

// When this is finished, advances to the first character after the symbol
static FreeSL_Token* TokenizeSymbol(FreeSL_Tokenizer* t, FreeSL_Scope* scope)
{
	int len = GetSymbolSize(t);
	FreeSL_String symbol;
	symbol.data = t->code;
	symbol.size = len;

	FreeSL_Var* var = FindVarInScope(scope, symbol);
	FreeSL_Function* func = FindFunction(t, symbol);
	FreeSL_Type* type = FindType(t, symbol);
	if (!var && !func && !type) FREESL_ERR("Unrecognized symbol", t->cur_char, t->cur_line);

	FreeSL_Token* tok = malloc(sizeof(FreeSL_Token));
	if (var)
	{
		tok->type = FSL_TOK_VAR;
		tok->var = var;
	}
	else if (type)
	{
		Advance(t, len);
		SkipWhitespace(t);
		if (t->code[0] == '(')
		{
			tok->type = FSL_TOK_CONSTRUCTOR;
			tok->target_type = type;
			TokenizeArgs(t, scope, tok, type->origin == FSL_TYPE_CUSTOM ? type->member_count : type->count);
			Advance(t, 1);
		}
		else if (IsSymbolChar(t->code[0]))
		{
			len = GetSymbolSize(t);
			
			FreeSL_Var* new_var = malloc(sizeof(FreeSL_Var));
			new_var->type = type;
			new_var->name.data = ResizeDataNoFree(t->code, len, len);
			new_var->name.size = len;

			FreeSL_Var* check_var = FindVarInScope(scope, new_var->name);
			FreeSL_Var* check_func = FindFunction(t, new_var->name);
			FreeSL_Var* check_type = FindType(t, new_var->name);
			if (check_var) FREESL_ERR("A variable already exists with this name", t->cur_char, t->cur_line);
			if (check_func) FREESL_ERR("A function already exists with this name", t->cur_char, t->cur_line);
			if (check_type) FREESL_ERR("A type already exists with this name", t->cur_char, t->cur_line);

			scope->vars[scope->vars_count++] = new_var;

			scope->vars = ResizeData(scope->vars, sizeof(FreeSL_Var) * scope->vars_count, sizeof(FreeSL_Var) * (scope->vars_count + 1));

			tok->type = FSL_TOK_VAR;
			tok->var = new_var;

			Advance(t, len);
		}
		else
		{
			FREESL_ERR("Expected constructor or variable declaration with type", t->cur_char, t->cur_line);
		}
	}
	else
	{
		tok->type = FSL_TOK_CALL;
		tok->function = func;
		tok->args = malloc(sizeof(FreeSL_Token*));

		Advance(t, len);
		SkipWhitespace(t);
		if (t->code[0] != '(') FREESL_ERR("Expected argument list for function call", t->cur_char, t->cur_line);

		TokenizeArgs(t, scope, tok, func->param_count);

		Advance(t, 1);
	}
	return tok;
}

// When calling, make sure there is no beginning whitespace
// When this is finished, `t->code` will ALWAYS be at the start of the next token
static FreeSL_Token* TokenizeSubExpr(FreeSL_Tokenizer* t, FreeSL_Scope* scope)
{
	FreeSL_Token* tok = 0;

	if (t->code[0] == '(')
	{
		int next = FindNextMatchingCharStop(t, '(', ')', LINE_DELIMITER);
		if (next == -1) FREESL_ERR("Unmatched parantheses", t->cur_char, t->cur_line);
		char* stop_at = t->code + next;
		Advance(t->code, 1);
		SkipWhitespaceSafe(t);
		tok = TokenizeExpr(t, scope, stop_at);
		Advance(t, 1);
	}
	else
	{
		// the order of these if's is important; it ensures it does not allow symbols to start with numbers
		if (IsDigit(t->code[0]))
		{
			tok = TokenizeNumber(t);
		}
		else if (IsSymbolChar(t->code[0])) 
		{
			tok = TokenizeSymbol(t, scope);
		}
		else
		{
			FREESL_ERR("Expected symbol or number", t->cur_char, t->cur_line);
		}
	}

	SkipWhitespaceSafe(t);

	return tok;
}

static void PopExprOrOp(FreeSL_ExprOrOp** list, int* size, int at)
{
	FreeSL_ExprOrOp* new_list = ResizeDataNoFree(*list, *size * sizeof(FreeSL_ExprOrOp), (*size - 1) * sizeof(FreeSL_ExprOrOp));

	for (int i = size - 1; i > at; i--)
	{
		new_list[i - 1] = (*list)[i];
	}
	
	free(*list);
	*size--;

	*list = new_list;
}

FreeSL_Token* TokenizeExpr(FreeSL_Tokenizer* t, FreeSL_Scope* scope, char* stop_at)
{
	FreeSL_ExprOrOp* list = malloc(sizeof(FreeSL_ExprOrOp));
	int list_size = 0;

	while (t->code < stop_at)
	{
		FreeSL_TokenType op = IdentifyOp(t);

		FreeSL_ExprOrOp item;
		if (op == FSL_TOK_INVALID)
		{
			item.expr = TokenizeSubExpr(t, scope);
			item.is_op = false;
		}
		else
		{
			item.op = op;
			item.is_op = true;
			Advance(t, GetOpLen(t));
		}

		list[list_size++] = item;
		list = ResizeData(list, list_size * sizeof(FreeSL_ExprOrOp), (list_size + 1) * sizeof(FreeSL_ExprOrOp));

		SkipWhitespace(t);
	}

	if (list_size == 0) FREESL_ERR("Cannot have empty expression", t->cur_char, t->cur_line);

	for (int cur_precedence = 0; cur_precedence <= MAX_PRECEDENCE; cur_precedence++)
	{
		if (list_size == 1) break;
		for (int i = list_size - 1; i >= 0; i--)
		{
			if (list[i].is_op)
			{
				if (GetPrecedence(list[i].op) != cur_precedence) continue;
				bool ExprBehind = false;
				bool ExprFront = false;

				if (i > 0) if (!list[i - 1].is_op) ExprBehind = true;
				if (i < list_size - 1) if (!list[i + 1].is_op) ExprFront = true;

				if (ExprFront == true && ExprBehind == false && IsOpPrefix(list[i].op))
				{
					FreeSL_Token* tok = malloc(sizeof(FreeSL_Token));
					tok->type = list[i].op;
					tok->expr = list[i + 1].expr;
					list[i].is_op = false;
					list[i].expr = tok;
					PopExprOrOp(&list, &list_size, i + 1);
				}
				else if (ExprFront == false && ExprBehind == true && IsOpPostfix(list[i].op))
				{
					FreeSL_Token* tok = malloc(sizeof(FreeSL_Token));
					tok->type = list[i].op;
					tok->expr = list[i - 1].expr;
					list[i].is_op = false;
					list[i].expr = tok;
					PopExprOrOp(&list, &list_size, i - 1);
					i--;
				}
				else if (ExprFront == true && ExprBehind == true && IsOpMiddle(list[i].op))
				{
					FreeSL_Token* tok = malloc(sizeof(FreeSL_Token));
					tok->type = list[i].op;
					tok->lhs = list[i - 1].expr;
					tok->rhs = list[i + 1].expr;
					list[i].is_op = false;
					list[i].expr = tok;
					PopExprOrOp(&list, &list_size, i + 1);
					PopExprOrOp(&list, &list_size, i - 1);
					i--;
				}
				else FREESL_ERR("Incorrect operator placement", t->cur_char, t->cur_line);
			}
		}
	}
	return list[0].expr;
}

// `*out_lines` has to be allocated with the size of 1 token already
// `*out_lines_size` must be 0
// When calling, make sure the start is the opening curly bracket
// When this is finished, `t->code` will ALWAYS be at the closing curly bracket
static void TokenizeScope(FreeSL_Tokenizer* t, FreeSL_Scope* scope, FreeSL_Token*** out_lines, int* out_lines_size)
{
	SkipWhitespace(t);
	if (t->code[0] != '{') FREESL_ERR("Code blocks must start with '{'", t->cur_char, t->cur_line);
	int match = FindNextMatchingChar(t, '{', '}');
	if (match == -1) FREESL_ERR("No matching '}' found", t->cur_char, t->cur_line);
	while (t->code[0] != '}')
	{
		Advance(t, 1);
		SkipWhitespace(t);

		if (t->code[0] == '}') break;

		int line_end = FindNextChar(t, ';');
		int code_block_end = FindNextChar(t, '}');

		if (line_end == -1) FREESL_ERR("Expressions must end with ';'", t->cur_char, t->cur_line);
		if (code_block_end < line_end) FREESL_ERR("Expressions must end with ';'", t->cur_char, t->cur_line);

		line_end = min(line_end, code_block_end);

		(*out_lines)[*out_lines_size++] = TokenizeExpr(t, scope, t->code + line_end);
		*out_lines = ResizeData(*out_lines, *out_lines_size * sizeof(FreeSL_Token*), (*out_lines_size + 1) * sizeof(FreeSL_Token*));

	}
}

// When calling, make sure there is no beginning whitespace
// When this is finished, `t->code` will ALWAYS be at the start of the next line
static void TokenizeRootLine(FreeSL_Tokenizer* t)
{
	int len = GetSymbolSize(t);
	FreeSL_String symbol;
	symbol.data = t->code;
	symbol.size = len;
	FreeSL_Type* type = FindType(t, symbol);

	if (!type) FREESL_ERR("Type not recognized", t->cur_char, t->cur_line);
	
	Advance(t, len);
	SkipWhitespace(t);

	len = GetSymbolSize(t);
	symbol.data = t->code;
	symbol.size = len;

	FreeSL_Var* check_var = FindVarInScope(&t->ast.root_scope, symbol);
	FreeSL_Function* check_func = FindFunction(t, symbol);
	FreeSL_Type* check_type = FindType(t, symbol);

	if (check_var) FREESL_ERR("A variable already exists with this name", t->cur_char, t->cur_line);
	if (check_func) FREESL_ERR("A function already exists with this name", t->cur_char, t->cur_line);
	if (check_type) FREESL_ERR("A type already exists with this name", t->cur_char, t->cur_line);

	FreeSL_RootLine line;

	if (TellNextChar(t, len) == '(')
	{
		Advance(t, len);
		SkipWhitespace(t);

		FreeSL_Function* new_func = malloc(sizeof(FreeSL_Function));
		new_func->name.data = ResizeData(symbol.data, len, len);
		new_func->name.size = len;
		new_func->return_type = type;
		new_func->scope->parent = &t->ast.root_scope;
		new_func->scope->vars = malloc(sizeof(FreeSL_Var*));
		new_func->scope->vars_count = 0;
		new_func->lines = malloc(sizeof(FreeSL_Token*));
		new_func->line_count = 0;
		new_func->param_count = 0;

		TokenizeParams(t, &t->ast.root_scope, new_func);

		Advance(t, 1);

		TokenizeScope(t, new_func->scope, &new_func->lines, &new_func->line_count);

		line.is_function = true;
		line.funcdecl = new_func;
	}
	else
	{
		FreeSL_Var* new_var = malloc(sizeof(FreeSL_Var));
		new_var->type = type;
		new_var->name.data = ResizeDataNoFree(t->code, len, len);
		new_var->name.size = len;

		FreeSL_Var* check_var = FindVarInScope(&t->ast.root_scope, new_var->name);
		FreeSL_Var* check_func = FindFunction(t, new_var->name);
		FreeSL_Var* check_type = FindType(t, new_var->name);
		if (check_var) FREESL_ERR("A variable already exists with this name", t->cur_char, t->cur_line);
		if (check_func) FREESL_ERR("A function already exists with this name", t->cur_char, t->cur_line);
		if (check_type) FREESL_ERR("A type already exists with this name", t->cur_char, t->cur_line);

		t->ast.root_scope.vars[t->ast.root_scope.vars_count++] = new_var;
		t->ast.root_scope.vars = ResizeData(t->ast.root_scope.vars, sizeof(FreeSL_Var) * t->ast.root_scope.vars_count, sizeof(FreeSL_Var) * (t->ast.root_scope.vars_count + 1));

		int line_end = FindNextChar(t, ';');
		if (line_end == -1) FREESL_ERR("Expected declaration to end with ';'", t->cur_char, t->cur_line);

		line.is_function = false;
		line.vardecl = TokenizeExpr(t, &t->ast.root_scope, t->code + line_end);
	}

	t->ast.roots[t->ast.roots_count++] = line;
	t->ast.roots = ResizeData(t->ast.roots, t->ast.roots_count * sizeof(FreeSL_RootLine), (t->ast.roots_count + 1) * sizeof(FreeSL_RootLine));

	Advance(t, 1);
}

FreeSL_AST FreeSL_GenAST(char* code)
{
	FreeSL_Tokenizer tokenizer;
	tokenizer.ast.roots = malloc(sizeof(FreeSL_RootLine));
	tokenizer.ast.roots_count = 0;
	tokenizer.ast.root_scope.parent = 0;
	tokenizer.ast.root_scope.vars = malloc(sizeof(FreeSL_Var));
	tokenizer.ast.root_scope.vars_count = 0;

	tokenizer.code = code;
	tokenizer.code_base = code;
	tokenizer.cur_char = 0;
	tokenizer.cur_line = 0;
	tokenizer.types = malloc(sizeof(FreeSL_Type) * 9);
	tokenizer.types_size = 9;

	char float_name[] = "float";
	char int_name[] = "int";
	char void_name[] = "void";
	char vec2_name[] = "vec2";
	char vec3_name[] = "vec3";
	char vec4_name[] = "vec4";
	char mat2_name[] = "mat2";
	char mat3_name[] = "mat3";
	char mat4_name[] = "mat4";

	tokenizer.types[0].name.data = float_name;
	tokenizer.types[0].name.size = 5;
	tokenizer.types[0].origin = FSL_TYPE_FLOAT;
	tokenizer.types[0].size = sizeof(float);

	tokenizer.types[1].name.data = int_name;
	tokenizer.types[1].name.size = 3;
	tokenizer.types[1].origin = FSL_TYPE_INT;
	tokenizer.types[1].size = sizeof(int);

	tokenizer.types[2].name.data = void_name;
	tokenizer.types[2].name.size = 4;
	tokenizer.types[2].origin = FSL_TYPE_VOID;
	tokenizer.types[2].size = 0;

	tokenizer.types[3].name.data = vec2_name;
	tokenizer.types[3].name.size = 4;
	tokenizer.types[3].origin = FSL_TYPE_VEC2;
	tokenizer.types[3].size = sizeof(float) * 2;

	tokenizer.types[4].name.data = vec3_name;
	tokenizer.types[4].name.size = 4;
	tokenizer.types[4].origin = FSL_TYPE_VEC3;
	tokenizer.types[4].size = sizeof(float) * 3;

	tokenizer.types[5].name.data = vec4_name;
	tokenizer.types[5].name.size = 4;
	tokenizer.types[5].origin = FSL_TYPE_VEC4;
	tokenizer.types[5].size = sizeof(float) * 4;

	tokenizer.types[6].name.data = mat2_name;
	tokenizer.types[6].name.size = 4;
	tokenizer.types[6].origin = FSL_TYPE_MAT2;
	tokenizer.types[6].size = sizeof(float) * 2 * 2;

	tokenizer.types[7].name.data = mat3_name;
	tokenizer.types[7].name.size = 4;
	tokenizer.types[7].origin = FSL_TYPE_MAT3;
	tokenizer.types[7].size = sizeof(float) * 3 * 3;

	tokenizer.types[8].name.data = mat4_name;
	tokenizer.types[8].name.size = 4;
	tokenizer.types[8].origin = FSL_TYPE_MAT4;
	tokenizer.types[8].size = sizeof(float) * 4 * 4;

	while (FindNextChar(&tokenizer, ';') != -1)
	{
		TokenizeRootLine(&tokenizer);
		SkipWhitespace(&tokenizer);
	}

	return tokenizer.ast;
}

int main()
{
	FILE* file = fopen("C:/Users/Andrei/Documents/Example.glsl", "r");

	fseek(file, 0, SEEK_END);
	int size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* fcontent = malloc(size + 1);
	fread(fcontent, 1, size, file);
	fcontent[size] = 0;

	FreeSL_AST ast = FreeSL_GenAST(fcontent);

	printf("Hello!");
}