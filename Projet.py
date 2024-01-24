import ply.yacc as yacc
import ply.lex as lex
from genereTreeGraphviz2 import printTreeGraph


reserved = {
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'print': 'PRINT',
    'while': 'WHILE',
    'do': 'DO',
    'end': 'END',
    'for': 'FOR',
    'function': 'FUNCTION',
}

tokens = [
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'SEMICOLON', 'COMMA', 'AND', 'OR', 'EQUAL', 'EQUALS', 'LOWER', 'HIGHER'
]+list(reserved.values())

# Tokens


def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'NAME')    # Check for reserved words
    return t


t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUAL = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_AND = r'\&'
t_OR = r'\|'
t_EQUALS = r'=='
t_LOWER = r'\<'
t_HIGHER = r'\>'
t_COMMA = r','


def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("\t\tInteger value too large %d", t.value)
        t.value = 0
    return t


# Ignored characters
t_ignore = " \t\t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("\t\tIllegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()


# Parsing rules


def p_start(t):
    ''' start : linst'''
    t[0] = ('start', t[1])
    # print("\t\t")
    # print(t[0])
    printTreeGraph(t[0])
    # eval(t[1])
    evalInst(t[1])


variables = {}
functions = {}


def evalInst(t):
    # print('\t\tevalInst', t)
    if type(t) != tuple:
        # print('\t\twarning : evalInst', t)
        return
    value = t[0]
    if value == 'if':
        expression, inst, elseInst = t[1], t[2], t[3]
        if evalExpr(expression):
            evalInst(inst)
        else:
            evalInst(elseInst)
    if value == 'while':
        expression, inst = t[1], t[2]
        while evalExpr(expression):
            evalInst(inst)
    if value == 'for':
        forAssign, forCondition, forInst, instructions = t[1], t[2], t[3], t[4]
        evalInst(forAssign)
        while evalExpr(forCondition):
            evalInst(instructions)
            evalInst(forInst)
    if value == 'function':
        name, params, instructions = t[1], t[2], t[3]
        if name in functions.keys():
            print("error : function already exists")
            return
        else:
            functions[name] = (params, instructions)
    if value == 'call':
        name, params = t[1], t[2]

        if name not in functions:
            print("error : function doesn't exist")
            return

        paramsFunction, instructions = functions[name][0], functions[name][1]

        if len(params) != len(paramsFunction):
            print("error : wrong number of parameters")
            return

        for i in range(len(params)):
            variables[paramsFunction[i]] = evalExpr(params[i])
        evalInst(instructions)
        for i in range(len(params)):
            del variables[paramsFunction[i]]
    if value == 'print':
        # handle multiple params
        if type(t[1]) == list:
            for param in t[1]:
                print('CONSOLE>', evalExpr(param))
        else:
            print('CONSOLE>', evalExpr(t[1]))
    if value == 'assign':
        variables[t[1]] = evalExpr(t[2])
    if value == 'bloc':
        evalInst(t[1])
        evalInst(t[2])
    if value == 'increment':
        variables[t[1]] += t[2]
    if value == 'decrement':
        variables[t[1]] -= t[2]
    
def evalExpr(t):
    # print('\t\tevalExpr', t)
    if type(t) == int:
        return t
    if type(t) == str:
        return variables[t]
    if t[0] == '+':
        return evalExpr(t[1])+evalExpr(t[2])
    if t[0] == '-':
        return evalExpr(t[1])-evalExpr(t[2])
    if t[0] == '*':
        return evalExpr(t[1])*evalExpr(t[2])
    if t[0] == '/':
        return evalExpr(t[1])/evalExpr(t[2])
    if t[0] == '<':
        return evalExpr(t[1]) < evalExpr(t[2])
    if t[0] == '>':
        return evalExpr(t[1]) > evalExpr(t[2])
    if t[0] == '&':
        return evalExpr(t[1]) and evalExpr(t[2])
    if t[0] == '|':
        return evalExpr(t[1]) or evalExpr(t[2])
    if t[0] == '==':
        return evalExpr(t[1]) == evalExpr(t[2])
    if t[0] == 'if':
        return evalExpr(t[1])
    if t[0] == 'else':
        return evalExpr(t[1])
    if t[0] == 'while':
        return evalExpr(t[1])
    if t[0] == 'do':
        return evalExpr(t[1])
    if t[0] == 'for':
        return evalExpr(t[1])
    if t[0] == 'end':
        return evalExpr(t[1])
    if t[0] == 'function':
        return evalExpr(t[1])
    if t[0] == 'call':
        return evalExpr(t[1])
    if t[0] == 'print':
        return evalExpr(t[1])
    if t[0] == 'assign':
        return evalExpr(t[2])
    if t[0] == 'bloc':
        return evalExpr(t[2])
    if t[0] == 'increment':
        return evalExpr(t[2])
    if t[0] == 'decrement':
        return evalExpr(t[2])
    

    return 0


def p_line(t):
    '''linst : linst inst 
            | inst '''
    if len(t) == 3:
        t[0] = ('bloc', t[1], t[2])
    else:
        t[0] = ('bloc', t[1], 'empty')

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_expression_number(t):
    'expression : NUMBER'
    t[0] = t[1]

def p_expression_name(t):
    'expression : NAME'
    t[0] = t[1]


def p_statement_assign(t):
    'inst : NAME EQUAL expression SEMICOLON'
    t[0] = ('assign', t[1], t[3])

def p_assign(t):
    '''assign : NAME EQUAL expression'''
    t[0] = ('assign', t[1], t[3])


# handle being incremented like x+=1 or x++
def p_increment(t):
    '''inst : NAME PLUS EQUAL NUMBER SEMICOLON
            | NAME PLUS PLUS SEMICOLON'''
    if len(t) == 6:
        t[0] = ('increment', t[1], t[4])
    else:
        t[0] = ('increment', t[1], 1)
 
def p_decrement(t):
    '''inst : NAME MINUS EQUAL NUMBER SEMICOLON
            | NAME MINUS MINUS SEMICOLON'''
    if len(t) == 6:
        t[0] = ('decrement', t[1], t[4])
    else:
        t[0] = ('decrement', t[1], 1)


def p_expression_list(t):
    '''expressions : expression COMMA expressions
              | expression'''
    if len(t) == 2:
        t[0] = [t[1]]
    else:
        t[0] = [t[1]]+t[3]

def p_statement_print(t):
    '''inst : PRINT LPAREN params RPAREN SEMICOLON
            | PRINT LPAREN expressions RPAREN SEMICOLON'''
    t[0] = ('print', t[3])

def p_comparison(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression OR expression
                  | expression AND expression
                  | expression EQUALS expression
                  | expression LOWER expression
                  | expression HIGHER expression
                  | expression DIVIDE expression'''
    t[0] = (t[2], t[1], t[3])

def p_condition(t):
    '''inst : IF LPAREN expression RPAREN THEN inst 
            | IF LPAREN expression RPAREN THEN inst ELSE inst'''
    if len(t) == 7:
        t[0] = ('if', t[3], t[6])
    else:
        t[0] = ('if', t[3], t[6], t[8])

def p_while(t):
    '''inst : WHILE LPAREN expression RPAREN DO linst END SEMICOLON'''
    t[0] = ('while', t[3], t[6])

def p_for(t):
    '''inst : FOR LPAREN assign SEMICOLON expression SEMICOLON assign RPAREN DO linst END SEMICOLON'''
    t[0] = ('for', t[3], t[5], t[7], t[10])

def p_params(t):
    '''params : NAME COMMA params
              | NAME'''
    if len(t) == 2:
        t[0] = [t[1]]
    else:
        t[0] = [t[1]]+t[3]

def p_funct(t):
    '''inst : FUNCTION NAME LPAREN params RPAREN linst END SEMICOLON
            | FUNCTION NAME LPAREN RPAREN linst END SEMICOLON'''
    name = t[2]
    params = t[4] if len(t) == 9 else []
    instructions = t[6] if len(t) == 9 else t[5]
    t[0] = ('function', name, params, instructions)

def p_call(t):
    '''inst : NAME LPAREN expressions RPAREN SEMICOLON
            | NAME LPAREN params RPAREN SEMICOLON
            | NAME LPAREN RPAREN SEMICOLON'''
    name = t[1]
    params = t[3] if len(t) == 6 else []
    t[0] = ('call', name, params)

def p_error(t):
    print("\t\tSyntax error at '%s'" % t.value)


parser = yacc.yacc()
s = open("prog.with",'r').read()
parser.parse(s)


# interpreter parametres fonctions et 2 petits bonus pour jeudi
