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


class Interpreter(object):
    """
    Main classs of our Interpreter
    It takes text(code snippet)

    self.pos ==> A pointer which points the current char in text
    self.currentChar ==> Current char read from text and stored in memory
    """
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.currentChar = self.text[self.pos]
        self.currentToken = self.getNextToken()

    def error(self, errType):
        if errType == "unknown":
            raise Exception("Unknown Char!")
        if errType == "wrongExp":
            raise Exception("Wrong Expression!")

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
        This function chesk currentChar' s type and use it to
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

    def eatToken(self, typeList):
        """
        This function call getNNextToken to create a Token from current
        char of text(code snippet) then she use the typeList to check
        if the newly created token' s type in expected token types list
        If not raises error.
        """
        token = self.getNextToken()

        if token.tokenType in typeList:
            return token
        else:
            self.error("wrongExp")

    def factor(self):
        token = self.eatToken(INT)
        literal = token.value
        return literal
    
    def term(self):
        result = self.factor()

        while self.currentToken is not None:
            token = self.eatToken([MUL, DIV])
            if token.tokenType == MUL:
                result *= self.factor()
            else:
                result /= self.factor()

        return result

    def expression(self):
        result = self.term()

        while self.currentToken is not None:
            token = self.eatToken([PLUS, MINUS])
            if token.tokenType == PLUS:
                result += self.term()
            else:
                result -= self.term()

        return result

def main():
    while True:
        try:
            text = input('ALQ_> ')
        except EOFError:
            break
        if not text:
            continue
        interpreter = Interpreter(text)
        result = interpreter.expression()
        print(result)

if __name__ == '__main__':
    main()
