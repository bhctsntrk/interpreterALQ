# Interpreter ALQ is a experimental interpreter project.

# Tokens ==> INT, PLUS, MINUS, WCHAR (White Char), EOF (End Of File)
INT = "INT"
PLUS = "PLUS"
MINUS = "MINUS"
WCHAR = "WCHAR"
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
        self.currentToken = None

    def error(self):
        raise Exception("Input Error!")

    def getNextToken(self):
        text = self.text
        if self.pos > len(text) - 1:
            return Token(EOF, None)

        currentChar = text[self.pos]

        if currentChar.isdigit():
            token = Token(INT, currentChar)
            self.pos += 1
            return token

        if currentChar == '+':
            token = Token(PLUS, currentChar)
            self.pos += 1
            return token

        if currentChar == '-':
            token = Token(MINUS, currentChar)
            self.pos += 1
            return token

        if currentChar == ' ':
            token = Token(WCHAR, currentChar)
            self.pos += 1
            return token

        self.error()

    def checkType(self, tokenType):
        if self.currentToken.tokenType == tokenType:
            return True
        else:
            return False

    def expression(self):
        self.left = None
        self.right = None
        self.operand = None
        

        while(True):
            self.currentToken = self.getNextToken()

            if self.checkType(INT):
                if self.left == None:
                    self.left = self.currentToken.value
                else:
                    if self.operand == None:
                        self.left = self.left + self.currentToken.value
                    else:
                        if self.right == None:
                            self.right = self.currentToken.value
                        else:
                            self.right = self.right + self.currentToken.value

            elif self.checkType(PLUS):
                self.operand = '+'
            elif self.checkType(MINUS):
                self.operand = '-'
            elif self.checkType(WCHAR):
                continue
            else:
                break
            
        result = None

        if self.operand == '+':
            result = int(self.left) + int(self.right)
        elif self.operand == '-':
            result = int(self.left) - int(self.right)
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
