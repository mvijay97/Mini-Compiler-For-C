import ply.lex as lex
import ply.yacc as yacc

scope_id=[]
ast_node_count=0
root_ast=0
class Scope:
  def __init__(self, parent):
    self.parent=parent
    self.tokens=dict()
    self.children=[]

class AST_Node:
  def __init__(self,parent,construct,terminal,value,array=False,length=0, dtype=None):
    self.parent = parent
    self.children = []
    self.construct = construct
    self.terminal = terminal
    self.value = value
    self.array = array # boolean flag for if an id corresponds to an array
    self.length = length # length of the array (Set to 0 for basic types)
    global ast_node_count
    self.label=ast_node_count
    self.dtype = dtype
    ast_node_count+=1

dtype_map = {'char':0,'int':1,'float':2}
dtype_inv = {0:'char',1:'int',2:'float'}

root_ast_node = AST_Node(parent = None,construct = 'root', terminal=False, value=None)
current_node = root_ast_node
print(current_node.construct)

main_scope = Scope(parent=None) #root scope parent=None
current_scope=main_scope
scope_level=0
newlinecount=0

tokens = ('AND','BACK_SLASH', 'CLOSE_CURLY_BRACES', 'CLOSE_PARANTHESES', 'CLOSE_SQUARE_BRACES', 
      'COLON', 'COMMA', 'DOLLAR', 'DOT', 'DOUBLE_QUOTES', 'EQUAL_TO', 'EXCLAMATORY_MARK', 
      'FORWARD_SLASH', 'GREATER_THAN', 'LESSER_THAN', 'MINUS', 
      'OPEN_CURLY_BRACES', 'OPEN_PARANTHESES', 'OPEN_SQUARE_BRACES', 'OR', 'PERCENTAGE', 
      'PLUS', 'POWER', 'QUESTION_MARK', 'SEMICOLON', 'SINGLE_QUOTES', 'STAR', 'UNDERSCORE',
      'PRE','CONST','WHILE','DO','IF','ELSE','VOLATILE','INT','CHAR','LONG_LONG','DOUBLE','FLOAT',
      'SIGNED','UNSIGNED','SHORT','LONG','T_STRUCT','T_UNION','COMMENT','ID','SEQ',
      'PLUS_PLUS','MINUS_MINUS','AND_AND','OR_OR','GREATER_THAN_EQUAL','LESSER_THAN_EQUAL','NOT_EQUAL_TO',
      'EQUAL_TO_EQUAL','PLUS_EQUAL_TO','MINUS_EQUAL_TO','LEFT_SHIFT_EQUAL_TO',
      'RIGHT_SHIFT_EQUAL_TO','AND_EQUAL_TO','OR_EQUAL_TO','POWER_EQUAL_TO',
      'FORWARD_SLASH_EQUAL_TO','PERCENTAGE_EQUAL_TO','LEFT_SHIFT','RIGHT_SHIFT',
      'STAR_EQUAL_TO','MAIN')

reserved = {'if':'IF', 'else':'ELSE', 'while':'WHILE', 'do':'DO', 'volatile':'VOLATILE',
      'int':'INT','char':'CHAR','double':'DOUBLE', 'float':'FLOAT', 'signed':'SIGNED', 
      'unsigned':'UNSIGNED','short': 'SHORT','long':'LONG','struct':'T_STRUCT',
      'union':'T_UNION','long long':'LONG_LONG'}


# t_MAIN=r'int\smain\(\)'
t_AND=r'&'
t_BACK_SLASH=r'\\'
t_CLOSE_CURLY_BRACES = r'\}'
t_CLOSE_PARANTHESES = r'\)'
t_CLOSE_SQUARE_BRACES = r'\]'
t_COLON = r':'
t_COMMA = r','
t_DOT = r'\.'
t_DOUBLE_QUOTES = r'"'
t_EQUAL_TO = r'='
t_EXCLAMATORY_MARK = r'!'
t_FORWARD_SLASH = r'/'
t_GREATER_THAN = r'\>'
t_LESSER_THAN = r'\<'
t_MINUS = r'\-'
t_OPEN_CURLY_BRACES = r'\{'
t_OPEN_PARANTHESES = r'\('
t_OPEN_SQUARE_BRACES = r'\['
t_OR = r'\|'
t_PERCENTAGE = r'%'
t_PLUS = r'\+'
t_POWER = r'\^'
t_QUESTION_MARK = r'\?'
t_SEMICOLON = r';'
t_SINGLE_QUOTES = r'\''
t_STAR = r'\*'
t_UNDERSCORE = r'_'
t_PRE = r'\#include\<stdio\.h\>'
#t_CONST #define function similar to t_NUMBER
t_ignore=' \t'
t_AND_AND=r'&&'
t_OR_OR=r'\|\|'
t_PLUS_PLUS=r'\+\+'
t_MINUS_MINUS=r'\-\-'
t_GREATER_THAN_EQUAL=r'\>='
t_LESSER_THAN_EQUAL=r'\<='
t_NOT_EQUAL_TO=r'!='
t_EQUAL_TO_EQUAL=r'=='
t_PLUS_EQUAL_TO=r'\+='
t_MINUS_EQUAL_TO=r'-='
t_STAR_EQUAL_TO=r'\*='
t_FORWARD_SLASH_EQUAL_TO=r'\/='
t_PERCENTAGE_EQUAL_TO=r'%='
t_LEFT_SHIFT=r'\<\<'
t_RIGHT_SHIFT=r'\>\>'
t_LEFT_SHIFT_EQUAL_TO=r'\<\<='
t_RIGHT_SHIFT_EQUAL_TO=r'\>\>='
t_AND_EQUAL_TO=r'&='
t_OR_EQUAL_TO=r'\|='
t_POWER_EQUAL_TO=r'\^='



precedence = (
  ('right','POWER_EQUAL_TO','OR_EQUAL_TO','AND_EQUAL_TO','RIGHT_SHIFT_EQUAL_TO',
    'LEFT_SHIFT_EQUAL_TO','PLUS_EQUAL_TO','MINUS_EQUAL_TO','STAR_EQUAL_TO',
    'FORWARD_SLASH_EQUAL_TO','EQUAL_TO','PERCENTAGE_EQUAL_TO'),
  ('left','OR_OR'),
  ('left','AND_AND'),
  ('left','OR'),
  ('left','POWER'),
  ('left','AND'),
  ('left','EQUAL_TO_EQUAL','NOT_EQUAL_TO'),
  ('left','LESSER_THAN_EQUAL','GREATER_THAN_EQUAL','LESSER_THAN','GREATER_THAN'),
  ('left','LEFT_SHIFT','RIGHT_SHIFT'),
  ('left', 'PLUS', 'MINUS'),
  ('left', 'STAR', 'FORWARD_SLASH'),
  ('left','PLUS_PLUS','MINUS_MINUS'),
)


def t_MAIN(t):
  r'int\smain\(\)'
  return(t)

def t_CONST(t):
    r'[-]?\d+[.]\d+E[-]?\d+|\d+[.]\d+|\d+'
    #r'\d+|\d+\.?\d*E\d+|\d+\.\d*|\d*\.\d+'
    try:
      if(t.value.find('.')>=0):
        t.value=float(t.value)
        # p7rint("FLOATED")
      else:
        t.value = int(t.value)
        # print("INTED")
    except ValueError:
        t.value = 0
    #print("VALUE IS \t\t",t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno+=len(t.value)


def t_ID(t):
  r'[a-zA-Z_][a-zA-Z_0-9]*'
  if(len(t.value)>31):
    print("Identifier ",t.value,' too long\n')
    t.value=t.value[0:31]
    print(t.value)
  t.type = reserved.get(t.value,'ID')
  if t.type=='ID':
    return t
  else:
    return t

def t_COMMENT(t):
    r'(/\*(.|\n)*?\*/)|(//.*)'
    pass

def t_error(t):
    t.lexer.skip(1)

def p_start(t):
    '''start : PRE MAIN stmt_grp'''
    # print("start")
    # print_tree(t[3])
    global root_ast
    root_ast=t[3]

def p_new_scope(t):
  '''new_scope : '''
  global current_scope
  global scope_level
  scope_level+=1
  new_scope = Scope(parent=current_scope)
  current_scope.children.append(new_scope)
  current_scope=new_scope

def p_new_node(t):
  '''new_node : '''
  print("node")
  global current_node
  new = AST_Node(parent = current_node,construct = 'stmt_grp', terminal=False, value=None)
  current_node = new

def p_stmt_grp(t):
  '''stmt_grp : OPEN_CURLY_BRACES CLOSE_CURLY_BRACES
  | OPEN_CURLY_BRACES new_scope new_node stmt_list CLOSE_CURLY_BRACES'''

  global current_scope
  global scope_level
  # print("SCOPE LEVEL: ",scope_level)
  scope_level-=1
  # print(current_scope.tokens)
  current_scope = current_scope.parent
  t[0] = t[4]
  

def p_stmt_list(t):
    '''stmt_list : stmt
    | stmt stmt_list'''   

    # switched stmt and stmt_list order in line 201
    if(len(t)==2):
      new = AST_Node(parent=None, construct='stmt_list', terminal=False, value=None)
      t[1].parent=new
      new.children.append(t[1])
      t[0] = new

    else:
      new = AST_Node(parent=None, construct='stmt_list', terminal=False, value=None)
      t[1].parent=new
      new.children.append(t[1])
      t[2].parent=new
      new.children.append(t[2])
      t[0] = new

def p_stmt(t):
    '''stmt : sel_stmt
    | iter_stmt
    | asgn_expr
    | stmt_grp
    | var_decl
    | expr_stmt
    '''
    # print("TREE")
    # print_tree(t[1])
    t[0] = t[1]

def p_expr_stmt(t):
  '''expr_stmt : expr SEMICOLON
  '''
  # print_tree(t[1])
  t[0] = t[1]

def p_expr(t):
  '''expr : bin_expr
     | paren_expr
     | primary_expr_id
     | primary_expr_const
     | un_expr
     | fun_call 
  '''
  t[0] = t[1]

def p_bin_expr(t):
  '''bin_expr : expr PLUS expr
              | expr MINUS expr
              | expr STAR expr
              | expr FORWARD_SLASH expr
              | expr PERCENTAGE expr
              | expr POWER expr
              | expr RIGHT_SHIFT expr
              | expr LEFT_SHIFT expr
              | expr GREATER_THAN expr
              | expr LESSER_THAN expr
              | expr GREATER_THAN_EQUAL expr
              | expr LESSER_THAN_EQUAL expr
              | expr EQUAL_TO_EQUAL expr
              | expr NOT_EQUAL_TO expr
              | expr OR_OR expr
              | expr AND_AND expr
              | expr AND expr
              | expr OR expr
              '''

  new = AST_Node(parent = None, construct='operator',value=t[2],terminal=True)
  t[1].parent = new 
  t[3].parent = new
  new.children.append(t[1])
  new.children.append(t[3])
  if(dtype_map[t[1].dtype]>dtype_map[t[3].dtype]):
    new.type = dtype_map[t[1].dtype]
  else:
    new.type = dtype_map[t[3].dtype]

  t[0] = new

def p_paren_expr(t):
  '''paren_expr : OPEN_PARANTHESES expr CLOSE_PARANTHESES'''
  t[0] = t[2]

def p_primary_expr_id(t):
  '''primary_expr_id : ID'''
  global current_scope
  id_key = (t[1],0)
  dtype = None
  if(id_key not in current_scope.tokens.keys()):
    temp=current_scope.parent
    while(temp!=None and  id_key not in temp.tokens.keys()):
        temp=temp.parent
    if(temp != None):
      temp.tokens[(t[1],0)]['use_count']+=1
      current_scope.tokens[(t[1],1)]={'parent_reference':temp,'scope_level':temp.tokens[id_key]['scope_number'],'line_no':t.lexer.lineno}
      dtype = temp.tokens[(t[1],0)]['type']
    else:
      print("NOT DECLARED")
      p_error(t)
  else:
    dtype = current_scope.tokens[(t[1],0)]['type']

  new = AST_Node(construct='terminal',parent=None,terminal=True,value=t[1], dtype=dtype)
  t[0] = new


def p_primary_expr_const(t):
  '''primary_expr_const : CONST'''

  new = AST_Node(construct='terminal',parent=None,terminal=True,value=t[1])
  # if('.' in t[1]):
  #   new.dtype = 'float'
  # else:
  #   new.dtype = 'int'
  t[0] = new

def p_fun_call(t):
    '''fun_call : ID OPEN_PARANTHESES param CLOSE_PARANTHESES'''
    new = AST_Node(parent=None, construct='fun_call',value=None,terminal=False)
    new.children.append(AST_Node(parent=new, construct='terminal', value=t[1],terminal=True))
    t[3].parent=new
    new.children.append(t[3])
    t[0] = new
    
def p_param(t):
  '''param : primary_param
  | primary_param COMMA param
  '''
  new = AST_Node(parent=None, construct='param_list',value=None,terminal=False)
  if(len(t)==2):
    if(t[1]):
      t[1].parent=new
      new.children.append(t[1])
      new.construct='primary_param'
    else:
      new.construct='empty_param'
  else:
    t[1].parent=new
    t[3].parent=new
    new.children.append(t[1])
    new.children.append(t[3])
  t[0] = new

def p_primary_param(t):
  '''primary_param : key_w_param
  | expr
  | empty
  '''
  t[0] = t[1]

def p_key_w_param(t):
  '''key_w_param : ID EQUAL_TO expr'''

  new = AST_Node(parent=None, construct='param',value='=',terminal=False)
  new.children.append(AST_Node(parent = new, construct='terminal',value=t[1],terminal=True))
  t[3].parent = new
  new.children.append(t[3])
  t[0] = new

def p_un_expr(t):
  '''un_expr : pre_op
  | post_op'''

  t[0]=t[1]

def p_pre_op(t):
  '''pre_op : PLUS_PLUS ID
  | MINUS_MINUS ID
  | EXCLAMATORY_MARK ID'''
  
  global current_scope
  id_key = (t[2],0)
  dtype=None
  if(id_key not in current_scope.tokens.keys()):
    temp=current_scope.parent
    while(temp!=None and  id_key not in temp.tokens.keys()):
        temp=temp.parent
    if(temp != None):
      temp.tokens[(t[2],0)]['use_count']+=1
      current_scope.tokens[(t[2],1)]={'parent_reference':temp,'scope_level':temp.tokens[id_key]['scope_number'],'line_no':t.lexer.lineno}
    else:
      print("NOT DECLARED")
      p_error(t)
  else:
    dtype = current_scope.tokens[(t[2],0)]['type']

  new = AST_Node(parent=None,construct='operator',value=t[1], terminal=False,dtype=dtype)
  new.children.append(AST_Node(parent=new, construct='terminal',value=t[2], terminal=True, dtype=dtype))
  t[0] = new

def p_post_op(t):
  '''post_op : ID PLUS_PLUS
  | ID MINUS_MINUS
  '''
  global current_scope
  id_key = (t[1],0)
  dtype = None
  # print(id_key,scope_level)
  if(id_key not in current_scope.tokens.keys()):
    temp=current_scope.parent
    while(temp!=None and  id_key not in temp.tokens.keys()):
        temp=temp.parent
    if(temp != None):
      temp.tokens[(t[1],0)]['use_count']+=1
      current_scope.tokens[(t[1],1)]={'parent_reference':temp,'scope_level':temp.tokens[id_key]['scope_number'],'line_no':t.lexer.lineno}
    else:
      print("NOT DECLARED")
  else:
    dtype = current_scope.tokens[(t[1],0)]['type']


  new = AST_Node(parent=None,construct='operator',value=t[2], terminal=False,dtype=dtype)
  new.children.append(AST_Node(parent=new, construct='terminal',value=t[1], terminal=True,dtype=dtype))
  t[0] = new


def p_var_decl(t):
  '''var_decl : qualifier signed_unsigned specifier type list SEMICOLON'''
  # print('p_var_decl')
  global scope_id
  global current_scope
  global scope_level
  for i in scope_id:
    if(i):
      current_scope.tokens[(i,0)]={'qualifier':t[1],'signed_unsigned':t[2], 'specifier':t[3],'type':t[4],'line_no':t.lexer.lineno,'scope_number':scope_level,'use_count':0}
  scope_id=[]

  new = AST_Node(parent=None,construct='var_declaration',value=None,terminal=False)
  new.children.append(AST_Node(parent=new,construct='var_type',value=t[4],terminal=True))
  t[5].parent = new
  # set_type(t[5],t[4])
  new.children.append(t[5])
  t[0] = new

def set_type(node,dtype):
  for i in node.children:
    if(i.construct == 'terminal'):
      i.dtype = dtype
    else:
      set_type(i,dtype)

def p_qualifier(t):
  '''qualifier  : CONST
  | VOLATILE 
  | empty'''

  t[0]=t[1]

def p_specifier(t):
  '''specifier : SHORT
  | LONG
  | LONG_LONG 
  | empty''' 
  t[0]=t[1]

def p_signed_unsigned(t):
  '''signed_unsigned : SIGNED
  | UNSIGNED
  | empty'''
  t[0]=t[1]

def p_empty(t):
  '''empty : '''
  pass

def p_type(t):
  '''type : basic
  | userdef'''
  # print('p_type')
  t[0]=t[1]

def p_basic(t):
  '''basic : INT
  | CHAR
  | DOUBLE
  | FLOAT'''
  # print('p_basic')
  t[0]=t[1]

def p_userdef(t):
  '''userdef : struct
  | union'''
  # print('p_userdef')

def p_struct(t):
  '''struct : T_STRUCT ID OPEN_CURLY_BRACES decl_list CLOSE_CURLY_BRACES'''
  # print('p_struct')

def p_decl_list(t):
  '''decl_list : var_decl SEMICOLON decl_list
  | SEMICOLON'''
  # print('p_decl_list')
  #print(t)

def p_union(t):
  '''union : T_UNION ID OPEN_CURLY_BRACES decl_list CLOSE_CURLY_BRACES'''
  # print('p_union')


def p_list(t):
  '''list : primary_list
          | primary_list COMMA list
  '''
  if(len(t)==2):
      new = AST_Node(parent=None, construct='decl_list', terminal=False, value=None)
      t[1].parent=new
      new.children.append(t[1])
      t[0] = new

  else:
    new = AST_Node(parent=None, construct='decl_list', terminal=False, value=None)
    t[1].parent=new
    new.children.append(t[1])
    t[3].parent=new
    new.children.append(t[3])
    t[0] = new


def p_primary_list(t):
  '''primary_list : ID
     | ID OPEN_SQUARE_BRACES expr CLOSE_SQUARE_BRACES
     | ID EQUAL_TO expr
     | ID OPEN_SQUARE_BRACES expr CLOSE_SQUARE_BRACES EQUAL_TO OPEN_CURLY_BRACES initialiser CLOSE_CURLY_BRACES 
  '''
  global scope_id
  if(isinstance(t[1],str) and t[1]!=''):
    scope_id.append(t[1])

  if(len(t) == 2): 
    new = AST_Node(parent=None, construct='terminal',value=t[1],terminal=True)
    t[0]=new

  elif (len(t) == 5):
    new = AST_Node(parent=None, construct='terminal',value=t[1],array=True,length=t[3])
    t[0]=new

  elif (len(t) == 4):
    new = AST_Node(parent=None, construct='assign',value = '=', terminal=False)
    new.children.append(AST_Node(parent=new, construct='terminal',value=t[1],terminal=True))
    t[3].parent=new
    new.children.append(t[3])
    t[0]=new

  else:
    new = AST_Node(parent=None, construct='assign', value = '=', terminal=False)
    new.children.append(AST_Node(parent=new, construct='terminal',value=t[1], terminal=True, array=True, length=t[3]))
    t[7].parent=new
    new.children.append(t[7])
    t[0]=new


def p_initialiser(t):
  '''initialiser : ID COMMA initialiser
    | CONST COMMA initialiser 
    | ID
    | CONST
  '''
  new = AST_Node(parent=None,construct='array_init',value=None,terminal=False)
  if(len(t)==4):
    new.children.append(AST_Node(parent=new,construct='init_value',value=t[1],terminal=True))
    t[3].parent=new
    new.children.append(t[3])
  else:
    new.children.append(AST_Node(parent=new,construct='init_value',value=t[1],terminal=True))
  t[0]=new


def p_sel_stmt(t):
  '''sel_stmt : IF OPEN_PARANTHESES expr CLOSE_PARANTHESES stmt ELSE stmt
  | IF OPEN_PARANTHESES asgn_expr CLOSE_PARANTHESES stmt ELSE stmt
  | IF OPEN_PARANTHESES expr CLOSE_PARANTHESES stmt
  | IF OPEN_PARANTHESES asgn_expr CLOSE_PARANTHESES stmt'''
  # print('p_sel_stmt')

def p_asgn_expr(t):
  '''asgn_expr : ID asgn_op expr SEMICOLON'''
  
  global current_scope
  id_key = (t[1],0)
  # print(id_key,scope_level)
  dtype = None
  if(id_key not in current_scope.tokens.keys()):
    temp=current_scope.parent
    while(temp!=None and  id_key not in temp.tokens.keys()):
        temp=temp.parent
    if(temp != None):
      temp.tokens[(t[1],0)]['use_count']+=1
      dtype = temp.tokens[(t[1],0)]['type']
      current_scope.tokens[(t[1],1)]={'parent_reference':temp,'scope_level':temp.tokens[id_key]['scope_number'],'line_no':t.lexer.lineno}
    else:
      print("NOT DECLARED")
      p_error(t)
  else:
    dtype = current_scope.tokens[(t[1],0)]['type']

  new = AST_Node(parent=None,construct='assign',value=t[2],terminal=True)
  new.children.append(AST_Node(parent=new,construct='terminal',terminal=True,value=t[1],dtype=dtype))
  t[3].parent=new
  new.children.append(t[3])

  if(dtype_map[new.children[0].dtype]<dtype_map[t[3].dtype]):
    print("WARNING: Cannot assign expression of type {} to {}".format(dtype_inv[dtype_map[t[3].dtype]],dtype_inv[dtype_map[new.children[0].dtype]]))
  t[0] = new

def p_asgn_op(t):
  '''asgn_op : EQUAL_TO
  | PLUS_EQUAL_TO
  | MINUS_EQUAL_TO
  | STAR_EQUAL_TO
  | FORWARD_SLASH_EQUAL_TO
  | PERCENTAGE_EQUAL_TO
  | LEFT_SHIFT_EQUAL_TO
  | RIGHT_SHIFT_EQUAL_TO
  | AND_EQUAL_TO
  | OR_EQUAL_TO
  '''
  t[0] = t[1]
  # print("p_asgn_op")

def p_iter_stmt(t):
  '''iter_stmt : WHILE OPEN_PARANTHESES expr CLOSE_PARANTHESES stmt
  | DO stmt_grp WHILE OPEN_PARANTHESES expr CLOSE_PARANTHESES SEMICOLON'''

  #print("WHILE",t[3].construct)
  if(len(t)==6):
    new = AST_Node(parent=None, construct = 'while', value=None, terminal=True)
    t[3].parent = new
    t[5].parent = new
    new.children.append(t[3])
    new.children.append(t[5])
  else:
    new = AST_Node(parent=None, construct = 'do_while', value=None, terminal=True)
    t[2].parent = new
    t[5].parent = new
    new.children.append(t[2])
    new.children.append(t[5])
  t[0] = new
  #print_tree(new)
# def p_error(t):
#   print("Syntax error at '%s'" % t)

def p_error(p):
  if(p):
      try:
          print("{1}: Syntax error on '{0}'".format(p.value, lexer.lineno))
      except AttributeError:
          print("{0}: Syntax error on line".format(lexer.lineno))
  else:
    p=p
lexer = lex.lex()

parser = yacc.yacc(start='start')
var=0
def print_tree(thisnode):
  try:
    print(thisnode.construct,thisnode.value, len(thisnode.children),thisnode.label)
    global var
    print("\t"*var,'{')
    var = var+1
    for i in thisnode.children:
      print("\t"*var,end="")
      print_tree(i)
    var = var - 1
    print("\t"*var,'}')
  except Exception as e:
    print(e)
TEMP=0
def bfs_labeler(root):
  bfslabel=1
  try:
    root.label=bfslabel
    q=[]
    for i in root.children:
      q.append(i)
    root.children
    while len(q)>0:
      t=q.pop(0)
      print(t.construct)
      t.label=bfslabel
      bfslabel=bfslabel+1
      if(len(t.children)>0):
        for l in range(len(t.children)):
          q.append(t.children[l])
  except Exception as e:
    print(e)
  print("DONE")
cp={}
  
def icg(root):
  global TEMP
  global cp
  t=[]
  istack=[]
  try:
    if(root.construct=='do_while'):
      print("LABEL ",root.children[0].label,":")
      icg(root.children[0])
      print('IF ',icg(root.children[1])," GOTO LABEL",(root.children[0].label))
      print("GOTO LABEL",(root.parent.label+1))
      print("LABEL",(root.parent.label+1),":")
    elif(root.construct=='while'):
      print("LABEL ",root.children[0].label,":")
      print('IF_FALSE ',icg(root.children[0])," GOTO LABEL",(root.parent.label+1))
      icg(root.children[1])
      print("GOTO LABEL",root.children[0].label)
      print("LABEL",(root.parent.label+1),":")
    elif(root.construct=='param_list'):
      print("PARAM ",icg(root.children[0]))
      if(root.children[1].construct=='param_list'):
        # t.append(
        icg(root.children[1])
        #)
      elif(root.children[1].construct=='primary_param'):
        # t.append(icg(root.children[1]))
        icg(root.children[1])
        # return icg(root.children[1].children[0])
    elif(root.construct=='primary_param'):
      print("PARAM",icg(root.children[0]))
    elif(root.construct=='empty_param'):
      t=t
    elif(root.construct=='fun_call'):
      # print(icg(root.children[1]))
      t.append(icg(root.children[1]))
      TEMP=TEMP+1
      print('T'+str(TEMP),' = ',"CALL ",icg(root.children[0]))
      # print("T is ",t)
    elif(len(root.children)>0):
      for i in root.children:
        t.append(icg(i))
    if(root.construct=='operator'):
      TEMP=TEMP+1
      s='T'+str(TEMP),' = '+str(t[0])+str(root.value)+str(t[1])
      istack.append(s)
      print('T'+str(TEMP),' = ',t[0],root.value,t[1])
      return('T'+str(TEMP))
    if(root.construct=='assign'):
      if(root.children[0].construct=='terminal'):
        if(root.children[0].value in cp.keys()):
          cp.pop(root.children[0].value,None)
      if(root.children[1].construct=='terminal'):
        # print(t[0],' = ',root.children[1].value)
        # print("(((((((((",root.children[0].value,root.children[1].value)
        cp[root.children[0].value]=root.children[1].value
        # print("<<<<",cp.keys(),cp.values())
        return(root.children[1].value)
      else:
        TEMP=TEMP+1
        print(t[0],' = ','T'+str(TEMP-1))
        return('T'+str(TEMP))
    if(root.construct=='terminal'):
      if(root.parent.construct=='assign' and root.parent.children[0]==root):
        return(root.value)
      elif(root.value in cp.keys()):
        return cp[root.value]
      return(root.value)
      
  except Exception as e:
    print(e)

while True:
    try:
        s2 = input()   # Use raw_input on Python 2
        s1 = open(s2,'r')
        s = s1.read()
        print(s)        
        # parser.parse(s, debug=True)
        parser.parse(s, debug=False)
        # print_tree(current_node)
        bfs_labeler(root_ast)
        # print_tree(root_ast)
        # icg(root_ast)
        
    except EOFError:
        break
 