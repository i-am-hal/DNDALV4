#[  Alastar Slater
    March 3, 2021
    parser.nim     

    Contains all of the functions
    and classes which will strictly
    parse the input, and produce an
    AST for the checker & interpreter   ]#
from strutils import isSpaceAscii, isDigit, toUpperAscii, parseInt, Letters
import strformat

type
    #All of the different types of tokens
    TokenType* = enum
        Eol
        Unknown
        Rparen
        Lparen
        Rbracket
        Lbracket
        Integer
        DiceRoll
        SumRoll
        Add
        Sub
        Mult
        Div
        Mod
        Sum
        LastResult
        Advantage
        Disadvantage
    
    #Definition of a token, which is a token w/ token type, value, and column position
    Token* = tuple[tokType: TokenType, value: string, column: int]
    
    #The lexer datatype, this will slowly chop input into lexemes (tokens)
    Lexer* = object 
        text: string 
        pos: int 
        chr: char 
        #toks: seq[Token]
        error: bool
    
    #Defines the parser, will take text, produce the AST
    Parser* = object
        lexer: Lexer
        currToken: Token 
        tokenIndex: int
        tokens: seq[Token]
        error*: bool
        errorStr*: string
    
    #Base for all nodes to be spewn from the Parser
    Node* = ref object of RootObj

    #NoOp does literally nothing.
    NoOp* = ref object of Node

    #Only variable, tells us to load in last result
    LastAnswer* = ref object of Node

    #An integer value.
    Number* = ref object of Node
        value*: int

    #A binary operation, takes two values and does an operation on them
    BinOp* = ref object of Node
        operator*: TokenType
        left*, right*: Node
    
    #Unary operation of some kind
    UnaryOp* = ref object of Node
        operator*: TokenType
        value*: Node
    
    #Some kind of dice roll, either sum roll, or regular roll
    Dice* = ref object of Node
        operator*: TokenType
        times*, sides*: Node
    
    #Just a series of nodes (values)
    List* = ref object of Node
        values*: seq[Node]
    

#==========[ LEXER METHODS ]==========#

#Makes a new instance of a lexer
func newLexer(text: string): Lexer =
    var firstChr = ' '

    #Gets the first character
    if len(text) > 0:
        firstChr = text[0]

    #Returns an instance of a lexer
    Lexer(text: text, pos: 0, chr: firstChr, error: false)

#Returns a new token tuple (used only by the lexer)
func newToken(tokenType: TokenType, tokValue: string, column: int): Token =
    return (tokType: tokenType, value: tokValue, column: column)

#Advances position forward, gets next character
proc advance(lexer: var Lexer) =
    lexer.pos += 1

    #If this is a valid index, get this char
    if lexer.pos < len(lexer.text):
        lexer.chr = lexer.text[lexer.pos]

#Advances lexer's POS by one, returns a token
proc newAdvToken(lexer: var Lexer, tokType: TokenTYpe, value: string): Token =
    lexer.advance()
    newToken(tokType, value, lexer.pos - 1)

#Skips over all the whitespaces
proc skipSpaces(lexer: var Lexer) =
    while lexer.chr.isSpaceAscii():
        lexer.advance()

#Collects all of the digits to an integer
proc getNumber(lexer: var Lexer): Token =
    var number = ""

    #Add each character onto this string
    while lexer.chr.isDigit() and lexer.pos < len(lexer.text):
        number.add(lexer.chr)
        lexer.advance()
    
    #Return the token for this integer token
    newToken(Integer, number, lexer.pos)

#Returns keyword token, kind of dice roll
proc getKeyword(lexer: var Lexer): Token =
    var keyword = ""
    let position = lexer.pos #Position of this keyword

    #Add each character to this keyword
    while lexer.chr in Letters and lexer.pos < len(lexer.text):
        keyword.add(lexer.chr)
        lexer.advance()
    
    #Make keyword fully uppercase
    keyword = keyword.toUpperAscii()

    #Simple roll, will produce a list of values
    if keyword == "D":
        return newToken(DiceRoll, keyword, position)

    #Sum roll, takes sum of all rolls
    elif keyword == "DS":
        return newToken(SumRoll, keyword, position)
    
    #Mod opreator, only the word version of it.
    elif keyword == "MOD":
        return newToken(Mod, keyword, position)

    #Sum keyword, return the Sum operator
    elif keyword == "SUM":
        return newToken(Sum, keyword, position)

    #If this is the variable for the last result
    elif keyword in @["RESULT", "ANS", "ANSWER"]:
        return newToken(LastResult, keyword, position)

    #if the user is wanting to roll advantage
    elif keyword in @["ADV", "ADVANTAGE"]:
        return newToken(Advantage, keyword, position)

    #If the user is wanting to roll disadvantage
    elif keyword in @["DIS", "DISADVANTAGE", "DISADV", "DISAD"]:
        return newToken(Disadvantage, keyword, position)

    else: #Otherwise, unknown keyword
        return newToken(Unknown, keyword, position)

#Attempts to get a new token and return it
proc nextToken(lexer: var Lexer): Token = 
    #Continue trying to get a token while a valid index
    while lexer.pos < len(lexer.text):
        let chr = lexer.chr #current char

        #Skip over all whitespace
        if chr.isSpaceAscii():
            lexer.skipSpaces()

        #If this is a number, get it
        elif chr.isDigit():
            return lexer.getNumber()

        #If this is a letter, return keyword
        elif chr in Letters:
            return lexer.getKeyword()

        #If this is a comment, then strip it out
        elif chr == ';':
            while chr != '\n' and lexer.pos < len(lexer.text):
                lexer.advance()

        #Left parenthesis
        elif chr == '(':
            return lexer.newAdvToken(Lparen, "(")

        elif chr == ')':
            return lexer.newAdvToken(Rparen, ")")

        elif chr == '[':
            return lexer.newAdvToken(Lbracket, "[")

        elif chr == ']':
            return lexer.newAdvToken(Rbracket, "]")

        #Takes any of the operators and makes it into a token
        elif chr in "+-*/%":
            #Gets the type for the operation
            let opTokType = 
                case chr:
                    of '+': Add 
                    of '-': Sub 
                    of '*': Mult
                    of '/': Div
                    else:   Mod 
            
            return lexer.newAdvToken(opTokType, $chr)

    newToken(Eol, "Eol", lexer.pos)

#Gets all of the tokens possible from lexing
proc getAllTokens(lexer: var Lexer): seq[Token] =
    var token = lexer.nextToken()

    #Continue adding tokens until out of tokens
    while token.tokType != Eol:
        result.add(token)
        token = lexer.nextToken()

    #Add final Eol token
    result.add(token)


#==========[ NODE CREATION PROCS ]==========#

#Creates a NoOp node
func newNoOp: Node = NoOp()

#Create a last result node
func newLastAnswer: Node = LastAnswer()

#Creates a new Number node
func newNumber(value: string): Node = Number(value: parseInt(value))

#Creates a new Binary Operation node
func newBinOp(operator: TokenType, left, right: Node): Node =
    BinOp(operator: operator, left: left, right: right)

#Create a new unary operator
func newUnaryOp(operator: TokenType, value: Node): Node =
    UnaryOp(operator: operator, value: value)

#Creates a new dice node
func newDice(operator: TokenType, times, sides: Node): Node =
    Dice(operator: operator, times: times, sides: sides)

#Creates a new list node
func newList(values: seq[Node]): Node = List(values: values)


#==========[ PARSER METHODS ]==========#

func toChar(op: TokenType): string =
    if op == Add:
        return "+"

    elif op == Sub:
        return "-"

    elif op == Mult:
        return "*"

    elif op == Mod:
        return "modulus"

    elif op == Div:
        return "/"

#Makes a new parser instance
proc newParser*(text: string): Parser =
    var 
        lexer = newLexer(text)
        tokens = lexer.getAllTokens()
        firstTok = lexer.newAdvToken(Eol, "Eol")
        errorState = false
        errorStr = ""
    
    #If there is space, get first token
    if len(tokens) > 0:
        firstTok = tokens[0]
    
    else: #Otherwise, set error
        errorState = true
        errorStr = "Eol encountered"

    #Return the instance of the parser
    Parser(lexer: lexer, currToken: firstTok, tokenIndex: 0, tokens: tokens, error: errorState, errorStr: errorStr)

#Removes current token if it matches, will get next token
proc eat(self: var Parser, expected: TokenType, error: string) =
    #If token types match, gets the next token in list
    if self.currToken.tokType == expected:
        self.tokenIndex += 1

        #If still within bounds, get the next token from list
        if self.tokenIndex < len(self.tokens):
            self.currToken = self.tokens[self.tokenIndex]
    
    #Otherwise, raises an error
    else:
        self.error = true
        self.errorStr = error

#Will try to eat and remove token w/ expected value. If fails, returns Noop node
template eatToken(self: var Parser, expected: TokenType, errorStr: string) =
    #If an error has ALREADY occured, don't even try this.
    if self.error:
        return newNoOp()

    #I there isn't already an error, try to remove next token
    else: 
        self.eat(expected, errorStr) #Tries getting rid of the current token.

        #If an error has occured, return noOp
        if self.error:
            return newNoOp()

#Forward declare expression so we can have recursive expressions
proc expression(self: var Parser): Node

#Creates a list, which is a series of expressions within brackets
proc list(self: var Parser): Node = 
    self.eatToken(Lbracket, "Expected left bracket to denote start of list.")
    var values: seq[Node]  #All of the values within the list

    #While not the end of the list, continue adding expressions to the total list
    while self.currToken.tokType notin [Rbracket, Eol] and not self.error:
        values.add(self.expression())
    
    #Try to remove the ending bracket for this list
    self.eatToken(Rbracket, "Expected right bracket to denote end of list.")
    #Return the list node of all of the expressions 
    newList(values)

#A base value: -UNIT, number, list, dice roll, sum of roll
proc unit(self: var Parser, nestedRoll = false): Node = 
    var 
        tokenType = self.currToken.tokType #Get current token type
        tokenValue = self.currToken.value

    #Parses the variable for the last variable
    if tokenType == LastResult:
        self.eatToken(LastResult, "Expected variable answer (for last result).")
        result = newLastAnswer()

    #If this is - x, then this is negation.
    elif tokenType == Sub:
        self.eatToken(Sub, "Expected negation (-) operator before term.")
        result = newUnaryOp(Sub, self.unit())
    
    #Sum operator, takes sum of a list of some kind.
    elif tokenType == Sum:
        self.eatToken(Sum, "Expected sum operator before term.")
        result = newUnaryOp(Sum, self.unit())
    
    #Takes advantage on the value, is a unary operator
    elif tokenType == Advantage:
        self.eatToken(tokenType, "Expected advantage operator.")
        result = newUnaryOp(Advantage, self.unit())
    
    #Takes disadvantage of the value, is unary operator
    elif tokenType == Disadvantage:
        self.eatToken(tokenType, "Expected disadvantage operator.")
        result = newUnaryOp(Disadvantage, self.unit())
    
    #If this is ( x ), then parse the sub expression
    elif tokenType == Lparen:
        self.eatToken(Lparen, "Expected open parenthesis before sub expression.")
        result = self.expression() #Get the sub expression that is inside the parens
        self.eatToken(Rparen, "Expected closing parenthesis after sub expression.")
    
    #If this is a number, parse it!
    elif tokenType == Integer:
        self.eatToken(Integer, "Expected an integer value.")
        result = newNumber(tokenValue)
    
    #If this is a list, parse it!
    elif tokenType == Lbracket:
        result = self.list()
    
    tokenType = self.currToken.tokType #Get current token type

    #Now that we have some manner of number of value, see if we have any dice!
    if tokenType in [DiceRoll, SumRoll] and not nestedRoll:
        self.eatToken(tokenType, "Expected a dice rolling operator (d, or ds).")
        #Creates a new expression for rolling this dice.
        result = newDice(DiceRoll, result, newUnaryOp(Sum, self.unit()))

        #Create a sum roll by having a regular roll within a sum operation
        if tokenType == SumRoll:
            result = newUnaryOp(Sum, result)

#Multiplies, Divides, or uses Mod on a series of units
proc term(self: var Parser): Node = 
    result = self.unit()

    while self.currToken.tokType in [Mult, Div, Mod] and self.currToken.tokType != Eol and not self.error:
        let op = self.currToken.tokType #get the operation performed
        #Remove operator token
        self.eatToken(op, fmt"Error, expected operator: {toChar(op)}")
        #Reshape result into being in a binary operation
        result = newBinOp(op, result, self.unit())

#Parses an expression, adds or subtracts two terms
proc expression(self: var Parser): Node =
    result = self.term()

    #Reshape into binary operations for every add, sub operation
    while self.currToken.tokType in [Add, Sub] and self.currToken.tokType != Eol and not self.error:
        let op = self.currToken.tokType #Get the operation being performed
        #Remove this token, move on
        self.eatToken(op, fmt"Error, expected operator: {toChar(op)}")
        #Reshape AST in terms of this operation
        result = newBinOp(op, result, self.term())

#Parses some computation, a valid expression
proc parse*(self: var Parser): Node = self.expression()

