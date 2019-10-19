# Interpreter ALQ is a experimental interpreter project.

# Tokens
INTEGER = "INTEGER"
REAL = "REAL"
INTNUM = "INTNUM"
REALNUM = "REALNUM"
PLUS = "PLUS"
MINUS = "MINUS"
MUL = "MUL"
DIV = "DIV"
INTDIV = "INTDIV"
EOF = "EOF"
OP = "OP"
CP = "CP"
BEGIN = "BEGIN"
END = "END"
DOT = "DOT"
COMMA = "COMMA"
COLON = "COLON"
ID = "ID"
ASSIGN = "ASSIGN"
SEMICOLON = "SEMICOLON"
PROGRAM = "PROGRAM"
VAR = "VAR"
PROCEDURE = "PROCEDURE"

# KEYWORDS
KEYWORDS = {
    "BEGIN": BEGIN,
    "END": END,
    "DIV": INTDIV,
    "PROGRAM": PROGRAM,
    "VAR": VAR,
    "INTEGER": INTEGER,
    "REAL": REAL,
    "PROCEDURE": PROCEDURE
}

"""
____________________________________________________________________
| LEXER ========================================================== |
| Lexer is do Tokenizing. Take the main code(input)                |
| then generate Tokens from the input                              |
L-------------------------------------------------------------------
"""


class Token(object):
    """
    We define our Token class in here
    Every Token has a value and type(like INT or PLUS)
    ___str___ and ___repr___ define what happen printed when we print our Token
    """
    def __init__(self, tokenType, value):
        self.tokenType = tokenType
        self.value = value

    def __str__(self):
        return "Token({}, {})".format(
            self.tokenType,
            repr(self.value)
        )

    def __repr__(self):
        return self.__str__()


class Lexer(object):
    """
    Lexer of our Interpreter
    It takes text(code snippet)

    self.pos ==> A pointer which points the current char in text
    self.currentChar ==> Current char read from text and stored in memory
    """
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.currentChar = self.text[self.pos]

    def error(self, errType):
        if errType == "unknown":
            raise Exception("Unknown Char!")

    def advanceRight(self):
        """
        After the getNextToken create a Token object from currentChar
        she needs to increase position point of text(code snippet)
        so call this function. This function also checks if the text end
        """
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.currentChar = None  # Indicates end of input
        else:
            self.currentChar = self.text[self.pos]

    def peekRight(self):
        """
        Sometimes to find differentiate between tokens we need
        to see second char(like tokens =>, ==)
        So this function peek to right characer but not advance
        """
        peekPos = self.pos + 1
        if peekPos > len(self.text) - 1:
            return None  # Indicates end of input
        else:
            return self.text[peekPos]

    def identifierToken(self):
        """
        This function controls identifiers. If first char is alpha
        this function called. Then this function merge all alphanums
        and return as a identifier name like (box1, test, START)
        The string that return can be a keyword.
        """
        ret = ""
        # Var names can start with underscore(_)
        while self.currentChar is not None and self.currentChar.isalnum() or\
                self.currentChar == "_":
            ret += str(self.currentChar)
            self.advanceRight()

        return ret

    def handleNumbers(self):
        """
        As you know we read text(code snippet) as a one char in every iteration
        however there are some numbers that wee need more than one char to
        define them. So we need this function to merge chars to
        create this numbers even they are floats and contains dot(.)
        """
        ret = ''
        isReal = False
        while self.currentChar is not None and self.currentChar.isdigit() or self.currentChar == '.':
            ret += str(self.currentChar)
            self.advanceRight()

            if self.currentChar == '.':
                isReal = True
        if isReal:
            return Token(REALNUM, float(ret))
        else:
            return Token(INTNUM, int(ret))

    def handleComment(self):
        """
        Advance right and ignore
        until to get '}' character.
        Then return empty
        """
        while self.currentChar != '}':
            self.advanceRight()
        return

    def getNextToken(self):
        """
        This function chesks currentChar' s type and use it to
        create a Token object and return it. If the char is White Space
        function skip to next char and does not create Token for White Space
        """
        while True:
            if self.currentChar is None:
                token = Token(EOF, None)
                return token

            elif self.currentChar.isalpha():
                identifier = self.identifierToken()
                # Return ID if identifier not keyword.
                # We use upper to ignore case sensivity in keywords
                if identifier.upper() in KEYWORDS:
                    token = \
                        Token(KEYWORDS[identifier.upper()], identifier.upper())
                else:
                    token = Token(ID, identifier)
                return token

            elif self.currentChar.isdigit():
                token = self.handleNumbers()
                return token

            elif self.currentChar == '{':
                self.handleComment()
                #  Move next char after "}"
                self.advanceRight()
                continue

            elif self.currentChar == '(':
                token = Token(OP, None)
                self.advanceRight()
                return token

            elif self.currentChar == ')':
                token = Token(CP, None)
                self.advanceRight()
                return token

            elif self.currentChar == '+':
                token = Token(PLUS, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == '-':
                token = Token(MINUS, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == '*':
                token = Token(MUL, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == '/':
                token = Token(DIV, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == '.':
                token = Token(DOT, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == ';':
                token = Token(SEMICOLON, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == ':' and self.peekRight() == '=':
                token = Token(ASSIGN, ":=")
                self.advanceRight()
                self.advanceRight()
                return token

            elif self.currentChar == ':':
                token = Token(COLON, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == ',':
                token = Token(COMMA, self.currentChar)
                self.advanceRight()
                return token

            elif self.currentChar == ' ':
                self.advanceRight()
                continue

            self.error("unknown")


"""
_______________________________________________________________________
| PARSER ==========================================================    |
| Parser is our main analyzer that take token stream from Lexer        |
| then try to generate valid expressions and calculations.             |
| It's output a AST Tree that made from those valid expressions        |
| There is also a syntax diagram as BNF that tell us grammar structure |
| Every Non-Terminal in that diagram has a method in parser that       |
| creates and builds AST Tree and it's nodes                           |
L----------------------------------------------------------------------
"""


"""
AST Tree Node Types Definitions.
This Nodes will be used to express calculations.
There can be a lot of them or none of them in a AST TreeS
"""


class ProgramNode(object):
    """
    Main program root node. Contains program name and block node
    Every AST Tree should have one.
    """
    def __init__(self, progName, block):
        self.name = progName
        self.block = block


class BlockNode(object):
    """
    Block node contains var declaration part
    plus compound part
    """
    def __init__(self, declarations, compounds):
        self.declarations = declarations
        self.compounds = compounds


class VarDeclarationNode(object):
    """
    This node uses for variable declarations in
    very beginning of the program
    """
    def __init__(self, VariableNode, variableType):
        self.variable = VariableNode
        self.variableType = variableType


class ProcedureDeclarationNode(object):
    """
    To define Procedure Declarations in AST Tree
    we need this. Every Precodure has a name
    and some parameters(maybe not) and an inner
    code block
    """
    def __init__(self, name, paramList, innerBlockNode):
        self.name = name
        self.paramList = paramList
        self.innerBlockNode = innerBlockNode


class Parameter(object):
    """
    This node used for defining procedure params
    Every parameter has a name and a type like
    Variables
    """
    def __init__(self, paramName, paramType):
        self.paramName = paramName
        self.paramType = paramType


class CompoundNode(object):
    """
    Compound Node contains a list that consist from statements.
    Statements are something like this a=5;
    or b = a + c;
    They finished with COMMA except the last statement before END
    keyword. It does not need a COMMA
    """
    def __init__(self, statementNodes):
        self.childs = statementNodes


class AssignmentNode(object):
    """
    This class define Assigment node.

    variable ASSIGN expression
        l      o         r
    """
    def __init__(self, variableNode, assignOp, expression):
        self.left = variableNode
        self.op = assignOp
        self.right = expression


class VariableNode(object):
    """
    Defines variable node
    """
    def __init__(self, variable):
        self.name = variable


class EmptyNode(object):
    """
    To defines empty program or compound
    """
    def __init__(self):
        pass


class BinaryNode(object):
    """
    Binary nodes represents operations in expressions
    3 + 5
    l o r
    """
    def __init__(self, left, operator, right):
        self.lNode = left
        self.opToken = operator
        self.rNode = right


class NumNode(object):
    """
    Num nodes represents numbers
    """
    def __init__(self, value):
        self.value = value


class UnaryNode(object):
    """
    Unary nodes represents unary operators
    Unary node has two thing first is operator
    Second is child. Child can be NumNode(Number)
    or another UnaryNode to express something like this
        5    -    -    -    4
       INT  MIN  MIN  MIN  INT
       INT  BNY  UNY  UNY  INT
       INT  BNY  UNY     INT
       INT  BNY       INT
     Factor    -     Factor
           EXPRESSION

    -     4 | Unary
    op    child
    """
    def __init__(self, operator, child):
        self.opToken = operator
        self.childToken = child


"""
MAIN SYNTAX DIAGRAM FOR PARSER ========================================

    program : PROGRAM variable SEMI block DOT

    block : declarations compound

    declarations : VAR (varDeclaration SEMI)+
                 | (PROCEDURE ID SEMI block SEMI)*
                 | empty

    varDeclaration : ID (COMMA ID)* COLON (INTEGER | REAL)

    compound : BEGIN statementList END

    statementList : statement
                   | statement SEMI statementList

    statement : compound
              | assignment
              | empty

    assignment : variable ASSIGN expr

    empty :

    expr : term ((PLUS | MINUS) term)*

    term : factor ((MUL | INTDIV | DIV) factor)*

    factor : PLUS factor
           | MINUS factor
           | INT
           | REAL
           | OP expr CP
           | variable

    variable: ID

"""


class Parser(object):
    """
    Parser of our Interpreter

    self.lexer = Lexer to get Tokens
    self.currentToken ==> Current Token

    currentToken is very important that we use
    too much when create the AST tree
    """
    def __init__(self, lexer):
        self.lexer = lexer
        self.currentToken = self.lexer.getNextToken()

    def error(self, errType):
        if errType == "wrongExp":
            raise Exception("Wrong Expression!")

    def eatToken(self, typeList):
        """
        This function firstly checks the type of currentToken if it can
        be founded in typeList function get next token and put to
        currentToken var.
        It will raises error if there are token type mismatch
        """
        if self.currentToken.tokenType in typeList:
            self.currentToken = self.lexer.getNextToken()
        else:
            self.error("wrongExp")

    def variable(self):
        """
        variable: ID

        Returns a variable node that contains variable name
        """
        variableNode = VariableNode(self.currentToken.value)
        self.eatToken(ID)
        return variableNode

    def factor(self):
        """
        factor : PLUS factor
               | MINUS factor
               | INT
               | REAL
               | OP expr CP
               | variable

        Factor is one of the main non-terminal in a statement
        normaly it is just a number or a variable
        """
        resultNode = None
        token = self.currentToken

        if self.currentToken.tokenType == INTNUM:
            self.eatToken(INTNUM)
            resultNode = NumNode(token.value)

        elif self.currentToken.tokenType == REALNUM:
            self.eatToken(REALNUM)
            resultNode = NumNode(token.value)

        elif self.currentToken.tokenType == PLUS:
            self.eatToken(PLUS)
            resultNode = UnaryNode(token, self.factor())

        elif self.currentToken.tokenType == MINUS:
            self.eatToken(MINUS)
            resultNode = UnaryNode(token, self.factor())

        # Call expression again Factor ->   (Literal | Identifier | (Expr))
        #                                                              ^
        elif self.currentToken.tokenType == OP:
            self.eatToken(OP)
            resultNode = self.expression()
            self.eatToken(CP)

        elif self.currentToken.tokenType == ID:
            resultNode = self.variable()

        return resultNode

    def term(self):
        """
        term : factor ((MUL | INTDIV | DIV) factor)*

        For sake of precedence level we need more non-terminals
        Expression and Term is not different actually
        """
        resultNode = self.factor()

        while self.currentToken.tokenType in [MUL, INTDIV, DIV]:
            token = self.currentToken
            if token.tokenType == MUL:
                self.eatToken(MUL)
            elif token.tokenType == DIV:
                self.eatToken(DIV)
            elif token.tokenType == INTDIV:
                self.eatToken(INTDIV)

            resultNode = BinaryNode(resultNode, token, self.factor())

        return resultNode

    def expression(self):
        """
        expr : term ((PLUS | MINUS) term)*

        Some operators have higher precedence so we need a
        non-terminal for every precedence level.

        So we use term and expr for (* /) and (+ -)

        The general rule is that if we have N levels of precedence
        we will need N + 1 non-terminals in total.

        BNF rule
            Expr ->     Term | Expr + Term | Expr – Term
            Term ->     Factor | Term * Factor | Term / Factor
            Factor ->
                (PLUS|MINUS)Factor | Literal | Identifier | (Expr) | variable

        Plus Minus can be founded in Term function
        Mul Div can be founded in Expr function
        because Mul and Div has priority
        """
        resultNode = self.term()

        while self.currentToken.tokenType in [PLUS, MINUS]:
            token = self.currentToken
            if token.tokenType == PLUS:
                self.eatToken(PLUS)
            elif token.tokenType == MINUS:
                self.eatToken(MINUS)

            resultNode = BinaryNode(resultNode, token, self.term())

        return resultNode

    def empty(self):
        """
        empty :

        Some non-terminals can be empty so we need this
        there will be error otherwise for
        Program a; Begin End.
        """
        return EmptyNode()

    def assignment(self):
        """
        assignment : variable ASSIGN expr

        We assign numbers to variables here
        """
        variableNode = self.variable()
        assignOp = self.currentToken.value
        self.eatToken(ASSIGN)
        expression = self.expression()

        assignmentNode = AssignmentNode(variableNode, assignOp, expression)

        return assignmentNode

    def statement(self):
        """
        statement : compound
                  | assignment
                  | empty

        Statements are normally look like this
        a := 5; or b := a + c;
        However there can be no assignment or
        there can be inner compound that starts with
        BEGIN so we handle this situation too in here.

        There can be nested BEGIN END's in code
        """
        if self.currentToken.tokenType == ID:
            assignmentNode = self.assignment()
            return assignmentNode

        elif self.currentToken.tokenType == BEGIN:
            compoundNode = self.compound()
            return compoundNode

        else:
            return EmptyNode()

    def statementList(self):
        """
        statementList : statement
                      | statement SEMI statement_list

        There can be infinite statements in one compund
        We use aother non-terminal for this.
        We can use this too
        compound : BEGIN (statement)* END
        """
        statementNodes = []

        statementNodes.append(self.statement())
        while self.currentToken.tokenType is SEMICOLON:
            self.eatToken(SEMICOLON)
            statementNodes.append(self.statement())

        return statementNodes

    def compound(self):
        """
        compound : BEGIN statementList END

        There are two part in Block non-terminal
        One is Declaration Part and other is compund
        part tha contains main code.

        """
        self.eatToken(BEGIN)
        root = CompoundNode(self.statementList())
        self.eatToken(END)
        return root

    def parameters(self):
        """
        parameters : ID (COMMA ID)* COLON (INTEGER | REAL)

        Because of pascal's design we need to split a part
        the parameter definition.
        foo (a, b : INTEGER; c : REAL;)
            ---------------  ---------
            parameters       parameters
            --------------------------
                 parameterList
        """
        parameterNodes = []
        paramNames = []
        paramNames.append(self.currentToken.value)
        self.eatToken(ID)
        while self.currentToken.tokenType == COMMA:
            self.eatToken(COMMA)
            paramNames.append(self.currentToken.value)
            self.eat(ID)

        self.eatToken(COLON)

        paramType = self.currentToken.value
        if self.currentToken.tokenType == INTEGER:
            self.eatToken(INTEGER)
        else:
            self.eatToken(REAL)
        for i in paramNames:
            parameterNodes.append(Parameter(i, paramType))
        return parameterNodes

    def parameterList(self):
        """
        parameterList : parameters
                      | parameters SEMI parameterList

        Parammeters and Procedure Declarations are part of
        Declaration non-terminal.
        """
        if self.currentToken.tokenType != ID:
            return []  # Function with no param
        parameters = []
        parameters = self.parameters()
        if self.currentToken == SEMICOLON:
            self.eatToken(SEMICOLON)
            [parameters.append(i) for i in self.parameterList()]
        return parameters

    def varDeclaration(self):
        """
        varDeclaration : ID (COMMA ID)* COLON (INTEGER | REAL)

        Variable Declaration another part of Declaration non-terminal
        Variables defined in this block.
        """
        varList = []
        while True:
            varList.append(self.currentToken.value)
            self.eatToken(ID)
            if self.currentToken.tokenType == COMMA:
                self.eatToken(COMMA)
                continue
            self.eatToken(COLON)
            if self.currentToken.tokenType == INTEGER:
                varDecNodes = \
                    [VarDeclarationNode(VariableNode(var), INTEGER) for var in varList]
                self.eatToken(INTEGER)
            else:
                varDecNodes = \
                    [VarDeclarationNode(VariableNode(var), REAL) for var in varList]
                self.eatToken(REAL)
            break

        return varDecNodes

    def declarations(self):
        """
        declarations : (VAR (varDeclaration SEMI)+)*
                     | (PROCEDURE ID (OP parameterList CP)? SEMI block SEMI)*
                     | empty

        Declaration one of the main non-terminal in block non-terminal
        Declaration + Compund = Block
        Variable declarations and procedure declarations done in here
        """
        declarationList = []
        # check if there is no declaration block
        if self.currentToken.tokenType == VAR:
            self.eatToken(VAR)

            while True:
                varDecNodes = self.varDeclaration()
                declarationList.append(varDecNodes)
                self.eatToken(SEMICOLON)

                if self.currentToken.tokenType != ID:
                    break

        if self.currentToken.tokenType == PROCEDURE:
            self.eatToken(PROCEDURE)
            procedureName = self.currentToken.value
            self.eatToken(ID)
            self.eatToken(OP)
            paramList = self.parameterList()
            self.eatToken(CP)
            self.eatToken(SEMICOLON)
            innerBlock = self.block()
            procedureNode = ProcedureDeclarationNode(procedureName, paramList, innerBlock)
            # ERROR WE NEED TO USE ANOTHER ARRAY BECAUSE OF VARDECLARATION
            declarationList.append([procedureNode])
            self.eatToken(SEMICOLON)

        return declarationList

    def block(self):
        """
        block : declarations compound

        Block is main body of program
        """
        declarations = self.declarations()
        compounds = self.compound()
        blockNode = BlockNode(declarations, compounds)
        return blockNode

    def program(self):
        """
        program : PROGRAM variable SEMI block DOT

        Main program starts with PROGRAM and ends with DOT
        """
        self.eatToken(PROGRAM)
        progName = self.variable().name
        self.eatToken(SEMICOLON)
        blockNode = self.block()
        root = ProgramNode(progName, blockNode)
        self.eatToken(DOT)
        return root

    def parse(self):
        return self.program()


"""
_______________________________________________________________________
| SEMANTIC ANALYZER ===============================================    |
| Semantic Analyzer analyze the AST tree before run-time.              |
| Get AST Tree from Parser                                             |
| then create Symbol Table and check some errors that parser           |
| can't check like nameDefineError.                                    |
| The nested scopes also handle in Semantic Analyzer                   |
L----------------------------------------------------------------------
"""


"""
Symbols Definitions.
Symbols are define the variables, built-ins, keywords
Before RunTime(Interpreting) we traverse the AST Tree
and find all variable declarations. Then we create a
symbols and put them together in a symbol table.
If there is undefined name error we can catch with
this semantic analyze thing
"""


class Symbol(object):
    """
    Symbol definiton.
    """
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class BuiltInSymbol(Symbol):
    """
    Definition for Built In Symbol
    """
    # Use super instead of write lot of same code
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.__str__()


class VarSymbol(Symbol):
    """
    Definition for Variable Symbol
    """
    # Use super instead of write lot of same code
    def __init__(self, name, type):
        self.name = name
        self.type = type

    def __str__(self):
        return "<{name}, {type}>".format(name=self.name, type=self.type)

    def __repr__(self):
        return self.__str__()


class ProcedureSymbol(Symbol):
    """
    Definition of Procedure Symbol
    Parameters need to be Variable Symbol
    Remember Pascal Procedures not has type like int
    Because they don't return something
    """
    def __init__(self, name, parameters=None):
        super().__init__(name)
        if parameters is None:
            self.parameters = []
        else:
            self.parameters = parameters

    def __str__(self):
        return "<{name}, {parameters}>".format(
            name=self.name,
            parameters=self.parameters
        )


class SymbolTable(object):
    """
    We create symbol tables from symbols in this class
    Scoping feature comes with procedures and their parameters
    controlled in this section.
    """
    def __init__(self, scopeName, scopeLevel, parentScope=None):
        self.symbols = {}
        self.scopeName = scopeName
        self.scopeLevel = scopeLevel
        self.parentScope = parentScope
        self.defineSymbol(BuiltInSymbol(INTEGER))
        self.defineSymbol(BuiltInSymbol(REAL))

    def __str__(self):
        print("/nScope Name", self.scopeName)
        print("Scope Level", self.scopeLevel)
        print("Parent Scope", self.parentScope.scopeName if self.parentScope else None)
        return "Symbols: {symbols}".format(
            symbols=[symbol for symbol in self.symbols.values()]
        )

    def __repr__(self):
        return self.__str__()

    def defineSymbol(self, symbol):
        """
        Add symbol to table
        """
        self.symbols[symbol.name] = symbol

    def checkSymbol(self, symbolName, onlyCurrentScope=False):
        """
        The process of mapping a variable reference
        to its declaration is called name
        resolution. And here is our checkSymbol
        method that does just that, name resolution:
        Note:! The onlyCurrentScope is for to catch
        error if some var defined more than one times
        in same scope
        """
        print('CheckSymbol: {}. (Scope name: {})'.format(symbolName, self.scopeName))
        if symbolName in self.symbols:
            return self.symbols[symbolName]
        elif self.parentScope is not None and not onlyCurrentScope:
            return self.parentScope.checkSymbol(symbolName)
        else:
            return None


class SemanticAnalyzer():
    """
    After parser build our AST tree, we traverse with
    SemanticAnalyzer before Interpreter and check
    Variable Declaration block. Because this block
    contains variable declarations we can create
    a symbol tree from here.
    """
    def __init__(self):
        self.currentScope = None

    def traverseTree(self, rootNode):
        """
        We traverse tree in here recursively
        Our target is catching the variable declarations
        and and name errors. And also we create nested scopes
        here.
        """
        if type(rootNode) == ProgramNode:
            """
            Main program and global scope here
            """
            print("Entering scope = Global scope!")
            globalScope = SymbolTable(
                scopeName="global",
                scopeLevel=1,
                parentScope=self.currentScope  # None
            )
            self.currentScope = globalScope
            self.traverseTree(rootNode.block)

            print(globalScope)

            self.currentScope = self.currentScope.parentScope
            print("Leaving scope = Global scope!")

        elif type(rootNode) == BlockNode:
            [self.traverseTree(varDeclaration) for declaration in rootNode.declarations for varDeclaration in declaration]
            self.traverseTree(rootNode.compounds)

        elif type(rootNode) == VarDeclarationNode:
            """
            We create variable symbols in here
            If the same var defined twice in same scope
            raises error
            """
            typeName = rootNode.variableType
            typeSymbol = self.currentScope.checkSymbol(typeName)
            VariableName = rootNode.variable.name
            variableSymbol = VarSymbol(VariableName, typeSymbol)

            if self.currentScope.checkSymbol(symbolName=VariableName, onlyCurrentScope=True):
                raise Exception("Error: Duplicate identifier '{name}' found".format(name=VariableName))

            self.currentScope.defineSymbol(variableSymbol)

        elif type(rootNode) == ProcedureDeclarationNode:
            """
            We define procedure nested scopes here
            and insert parameters from node to that scope and
            definition of procedureSymbol.parameters
            """
            procedureName = rootNode.name
            procedureSymbol = ProcedureSymbol(procedureName)
            self.currentScope.defineSymbol(procedureSymbol)

            # Define scope
            print("Entering scope = {scopeName} scope!".format(scopeName=procedureName))
            procedureScope = SymbolTable(
                scopeName=procedureName,
                scopeLevel=self.currentScope.scopeLevel + 1,
                parentScope=self.currentScope)
            self.currentScope = procedureScope

            for parameter in rootNode.paramList:
                parameterType = self.currentScope.checkSymbol(parameter.paramType)
                parameterName = parameter.paramName
                variableSymbol = VarSymbol(parameterName, parameterType)
                self.currentScope.defineSymbol(variableSymbol)
                procedureSymbol.parameters.append(variableSymbol)

            self.traverseTree(rootNode.innerBlockNode)

            print(procedureScope)

            self.currentScope = self.currentScope.parentScope
            print("Leaving scope = {scopeName} scope!".format(scopeName=procedureName))

        elif type(rootNode) == CompoundNode:
            [self.traverseTree(statement) for statement in rootNode.childs]

        elif type(rootNode) == AssignmentNode:
            variableName = rootNode.left.name
            variableSymbol = self.currentScope.checkSymbol(variableName)

            if variableSymbol is None:
                raise NameError(repr(variableName))

        elif type(rootNode) == VariableNode:
            variableName = rootNode.name
            variableSymbol = self.currentScope.checkSymbol(variableName)

            if variableSymbol is None:
                raise NameError(repr(variableName))

        elif type(rootNode) == BinaryNode:
            self.traverseTree(rootNode.lNode)
            self.traverseTree(rootNode.rNode)

        elif type(rootNode) == UnaryNode:
            self.traverseTree(rootNode.childToken)

        else:
            pass


"""
_______________________________________________________________________
| INTERPRETER =====================================================    |
| The main interpreter.                                                |
| After AST Tree builded and Semantic Analyze Completed, Run time begun|
| Interpreter traverse the tree and do calculations and change vars    |
| If tere is special functions like write to screen do that things     |
L----------------------------------------------------------------------
"""


class Interpreter(object):
    """
    Interpreter get a ASC tree from parser
    to calculate tree she use postorder traversal.
    """
    def __init__(self, parser):
        self.parser = parser
        self.variableStorage = {}

    def error(self, errType):
        if errType == "unknownNode":
            raise Exception("Unknown Node!")
        elif errType == "unknownOp":
            raise Exception("Unknown Operator!")
        elif errType == "noVariable":
            raise Exception("Undefined Variable Name")

    def traverseTree(self, rootNode):
        """
        This function traverse the tree in postorder
        recursively.
        """
        if type(rootNode) == ProgramNode:
            self.traverseTree(rootNode.block)

        elif type(rootNode) == BlockNode:
            [self.traverseTree(varDeclaration) for declaration in rootNode.declarations for varDeclaration in declaration]
            self.traverseTree(rootNode.compounds)

        elif type(rootNode) == VarDeclarationNode:
            pass

        elif type(rootNode) == ProcedureDeclarationNode:
            pass

        elif type(rootNode) == CompoundNode:
            [self.traverseTree(statement) for statement in rootNode.childs]

        elif type(rootNode) == AssignmentNode:
            variable = rootNode.left.variable
            expression = rootNode.right

            expression = self.traverseTree(expression)
            self.variableStorage[variable] = expression

        elif type(rootNode) == VariableNode:
            variable = rootNode.variable
            if variable in self.variableStorage:
                return self.variableStorage[variable]
            else:
                self.error("noVariable")

        elif type(rootNode) == EmptyNode:
            pass

        elif type(rootNode) == BinaryNode:
            left = self.traverseTree(rootNode.lNode)
            right = self.traverseTree(rootNode.rNode)
            operator = rootNode.opToken

            if operator.tokenType == PLUS:
                return left + right
            elif operator.tokenType == MINUS:
                return left - right
            elif operator.tokenType == MUL:
                return left * right
            elif operator.tokenType == DIV:
                return left / right
            elif operator.tokenType == INTDIV:
                return left // right
            else:
                self.error("unknownOp")

        elif type(rootNode) == NumNode:
            return rootNode.value

        elif type(rootNode) == UnaryNode:
            if rootNode.opToken.tokenType == PLUS:
                return +1 * self.traverseTree(rootNode.childToken)
            elif rootNode.opToken.tokenType == MINUS:
                return -1 * self.traverseTree(rootNode.childToken)

        else:
            self.error("unknownNode")

    def interpret(self):
        rootNode = self.parser.parse()

        semanticAnalyzer = SemanticAnalyzer()
        semanticAnalyzer.traverseTree(rootNode)

        self.traverseTree(rootNode)


def main():
    while True:
        try:
            text = input('ALQ_> ')
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        interpreter.interpret()
        print(interpreter.variableStorage)


if __name__ == '__main__':
    main()
