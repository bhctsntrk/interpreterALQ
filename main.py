# Interpreter ALQ is a experimental interpreter project.

# Tokens ==> INT, PLUS, MINUS, WCHAR (White Char), EOF (End Of File)
INT = "INT"
PLUS = "PLUS"
MINUS = "MINUS"
MUL = "MUL"
DIV = "DIV"
# WCHAR = "WCHAR"       Do I really need this?
EOF = "EOF"


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
        return "Token({tokenType}, {value}".format(
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

    def integerToken(self):
        """
        As you know we read text(code snippet) as a one char in every iteration
        however there are some Integers that wee need more than one char to
        define them (10 and every other INTs that bigger than 10 and
        also negative ones) So wee need this function to merge chars to
        create this integers
        """
        ret = ''
        while self.currentChar is not None and self.currentChar.isdigit():
            ret += str(self.currentChar)
            self.advanceRight()

        return int(ret)

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

            if self.currentChar.isdigit():
                bigInt = self.integerToken()
                token = Token(INT, bigInt)
                return token

            if self.currentChar == '+':
                token = Token(PLUS, self.currentChar)
                self.advanceRight()
                return token

            if self.currentChar == '-':
                token = Token(MINUS, self.currentChar)
                self.advanceRight()
                return token

            if self.currentChar == '*':
                token = Token(MUL, self.currentChar)
                self.advanceRight()
                return token

            if self.currentChar == '/':
                token = Token(DIV, self.currentChar)
                self.advanceRight()
                return token

            if self.currentChar == ' ':
                self.advanceRight()
                continue

            self.error("unknown")


class Parser(object):
    """
    Parser of our Interpreter
    Check and calculate expressions. She uses
    lexer to split up the text(code snippet)
    then cheks the Tokens orders if they are
     true expressions and calculable or not

    self.lexer = Lexer to get Tokens
    self.currentToken ==> Current Token
    """
    def __init__(self, lexer):
        self.lexer = lexer
        self.currentToken = None

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
            self.currentToken = lexer.getNextToken()
        else:
            self.error("wrongExp")

    def factor(self):
        """
        BNF rule
            Expr ->     Term | Expr + Term | Expr – Term
            Term ->     Factor | Term * Factor | Term / Factor
            Factor ->   Literal | Identifier | (Expr)

        EBNF equivalent
            Expr ->     Term { [+|-] Term }
            Term ->     Factor { [* | / ] Factor }
            Factor ->   Literal | Identifier | (Expr)

        RegEx equivalent
            Expr ->     (Term | Expr + Term | Expr - Term)
            Term ->     (Factor | Term * Factor | Term / Factor)
            Factor ->   (Literal | Identifier | (Expr))

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
        literal = self.currentToken.value
        self.eatToken(INT)
        return literal

    def term(self):
        result = self.factor()

        while self.currentToken.tokenType is not EOF:
            if self.currentToken.tokenType == MUL:
                self.eatToken(MUL)
                result *= self.factor()
            elif self.currentToken.tokenType == DIV:
                self.eatToken(DIV)
                result /= self.factor()
            else:
                break

        return result

    def expression(self):
        self.currentToken = self.getNextToken()
        result = self.term()

        while self.currentToken.tokenType is not EOF:
            if self.currentToken.tokenType == PLUS:
                self.eatToken(PLUS)
                result += self.term()
            elif self.currentToken.tokenType == MINUS:
                self.eatToken(MINUS)
                result -= self.term()
            else:
                self.error("wrongExp")

        return result


def main():
    while True:
        try:
            text = input('ALQ_> ')
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer()
        parser = Parser(lexer)
        result = parser.expression()
        print(result)

if __name__ == '__main__':
    main()
