# Interpreter ALQ is a experimental interpreter project.

# Tokens ==> INT, PLUS, MINUS, WCHAR (White Char), EOF (End Of File)
INT = "INT"
PLUS = "PLUS"
MINUS = "MINUS"
# WCHAR = "WCHAR"       Do I really need this?
EOF = "EOF"


class Token(object):
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
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.currentChar = self.text[self.pos]

    def error(self, errType):
        if errType == "unknown":
            raise Exception("Unknown Char!")
        if errType == "wrongExp":
            raise Exception("Wrong Expression!")

    def advanceRight(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.currentChar = None  # Indicates end of input
        else:
            self.currentChar = self.text[self.pos]

    def integerToken(self):
        ret = ''
        while self.currentChar is not None and self.currentChar.isdigit():
            ret += str(self.currentChar)
            self.advanceRight()

        return int(ret)

    def getNextToken(self):
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

            if self.currentChar == ' ':
                self.advanceRight()
                continue

            self.error("unknown")

    def eatToken(self, typeList):
        token = self.getNextToken()

        if token.tokenType in typeList:
            return token
        else:
            self.error("wrongExp")

    def expression(self):
        """ INT PLUS/MINUS INT
        """
        left = None
        right = None
        operand = None
        eof = None

        left = self.eatToken(INT)

        operand = self.eatToken([PLUS, MINUS])

        right = self.eatToken(INT)

        eof = self.eatToken(EOF)

        if operand.tokenType == PLUS:
            result = left.value + right.value
        elif operand.tokenType == MINUS:
            result = left.value - right.value
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
