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

# KEYWORDS
KEYWORDS = {
    "BEGIN": BEGIN,
    "END": END,
    "DIV": INTDIV,
    "PROGRAM": PROGRAM,
    "VAR": VAR,
    "INTEGER": INTEGER,
    "REAL": REAL
}


# LEXER ==========================================


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

            if self.currentChar is '.':
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

# PARSER ==========================================
# This classes are ASC tree node definitions.


class ProgramNode(object):
    """
    Main program root node. Contains program name and block node
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


class CompoundNode(object):
    """
    Compound Node contains a list consist from statements.
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
        self.variable = variable


class EmptyNode(object):
    """
    Defines empty compound like BEGIN END
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


class Symbol(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type


class BuiltInSymbol(Symbol):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.__str__()


class VarSymbol(Symbol):
    def __init__(self, name, type):
        self.name = name
        self.type = type

    def __str__(self):
        return "<{name}, {type}>".format(name=self.name, type=self.type)

    def __repr__(self):
        return self.__str__()


class Parser(object):
    """
    Parser of our Interpreter
    Check and calculate expressions. She uses
    lexer to split up the text(code snippet)
    then cheks the Tokens orders if they are
    true expressions and calculable or not.

    Then she creates an ASC Tree and return it

    Example =   1 + (3 + (-2))
    Tree =

        +(BinaryNode)
       / \
      1   +(BinaryNode)
         / \
        3  UNARY(-)
             \
              2(NumNode)

    Example = BEGIN BEGIN number := 2; a := number;
     b := 10 * a + 10 * number / 4; c := a - - b END; x := 11; END.
    Tree =

                                         Compound _____
                                            /    \     \
                                   Compound       \    |
                                 /                 /   \
                                                  :=    EmptyNode
                                                  / \
                                                 x  11


    self.lexer = Lexer to get Tokens
    self.currentToken ==> Current Token
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

    def factor(self):
        """
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

        RegEx equivalent
            Expr ->     (Term | Expr + Term | Expr - Term)
            Term ->     (Factor | Term * Factor | Term / Factor)
            Factor ->
                ((PLUS|MINUS)Factor | Literal | Identifier | (Expr) | variable

        The main logic in the following three function we try to
        find split our Tokens to Expr Term and Factor

        For 2+3+4

        We think 2 is Literal and 2+ is a Term
        after following Tokens after 2+ all of then are another Term

          2 +       3 + 4 .....bla bla
          - -       - - - ------
          F F       F F F FFFFFFF
        -------    --------------------
          Term          Term
        -------------------------------
                Expr

        There are infinite possobilities after 2+
        so we put it in a while loop everthing
        need to be calculated. After calculation complete we add value to 2

        Plus Minus can be founded in Term function
        Mul Div can be founded in Expr function
        because Mul and Div has priority

        """
        literalNode = None
        token = self.currentToken

        if self.currentToken.tokenType == INTNUM:
            self.eatToken(INTNUM)
            literalNode = NumNode(token.value)

        elif self.currentToken.tokenType == REALNUM:
            self.eatToken(REALNUM)
            literalNode = NumNode(token.value)

        elif self.currentToken.tokenType == PLUS:
            self.eatToken(PLUS)
            literalNode = UnaryNode(token, self.factor())

        elif self.currentToken.tokenType == MINUS:
            self.eatToken(MINUS)
            literalNode = UnaryNode(token, self.factor())

        # Call expression again Factor ->   (Literal | Identifier | (Expr))
        #                                                              ^
        elif self.currentToken.tokenType == OP:
            self.eatToken(OP)
            literalNode = self.expression()
            self.eatToken(CP)

        elif self.currentToken.tokenType == ID:
            literalNode = self.variable()

        return literalNode

    def term(self):
        resultNode = self.factor()

        while self.currentToken.tokenType is not EOF:
            token = self.currentToken
            if token.tokenType == MUL:
                self.eatToken(MUL)
            elif token.tokenType == DIV:
                self.eatToken(DIV)
            elif token.tokenType == INTDIV:
                self.eatToken(INTDIV)
            else:
                break

            resultNode = BinaryNode(resultNode, token, self.factor())

        return resultNode

    def expression(self):
        resultNode = self.term()

        while self.currentToken.tokenType is not EOF:
            token = self.currentToken
            if token.tokenType == PLUS:
                self.eatToken(PLUS)
            elif token.tokenType == MINUS:
                self.eatToken(MINUS)
            # Break the expression and force to return a value
            else:
                break

            resultNode = BinaryNode(resultNode, token, self.term())

        return resultNode

    def empty(self):
        # Empty Node
        return EmptyNode()

    def variable(self):
        # Return a variable node
        variableNode = VariableNode(self.currentToken.value)
        self.eatToken(ID)
        return variableNode

    def assignment(self):
        variableNode = self.variable()
        assignOp = self.currentToken.value
        self.eatToken(ASSIGN)
        expression = self.expression()

        assignmentNode = AssignmentNode(variableNode, assignOp, expression)

        return assignmentNode

    def statement(self):
        if self.currentToken.tokenType == ID:
            assignmentNode = self.assignment()
            return assignmentNode

        elif self.currentToken.tokenType == BEGIN:
            compoundNode = self.compound()
            return compoundNode

        else:
            return EmptyNode()

    def statement_list(self):
        statementNodes = []

        statementNodes.append(self.statement())
        while self.currentToken.tokenType is SEMICOLON:
            self.eatToken(SEMICOLON)
            statementNodes.append(self.statement())

        if self.currentToken.tokenType is ID:
            self.error("wrongExp")

        return statementNodes

    def compound(self):
        self.eatToken(BEGIN)
        root = CompoundNode(self.statement_list())
        self.eatToken(END)
        return root

    def varDeclaration(self):
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

        return declarationList

    def block(self):
        declarations = self.declarations()
        compounds = self.compound()
        blockNode = BlockNode(declarations, compounds)
        return blockNode

    def program(self):
        self.eatToken(PROGRAM)
        progName = self.variable().variable
        self.eatToken(SEMICOLON)
        blockNode = self.block()
        root = ProgramNode(progName, blockNode)
        self.eatToken(DOT)
        return root

    def parse(self):
        return self.program()

# INTERPRETER ===================================


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
        Get root node as param and check the type
        if NumNode just return the value
        If binary calculate the expression
        If unary change numbers' s sign
        """
        if type(rootNode) == ProgramNode:
            self.traverseTree(rootNode.block)

        elif type(rootNode) == BlockNode:
            [self.traverseTree(varDeclaration) for declaration in rootNode.declarations for varDeclaration in declaration]
            self.traverseTree(rootNode.compounds)

        elif type(rootNode) == VarDeclarationNode:
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
