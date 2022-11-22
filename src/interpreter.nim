#[  Alastar Slater
    April 2, 2021
    interpreter.nim

    Contains the type checker, as well as
    all of the routines and methods for
    strictly interpreting the AST produced
    by the parser to arrive at a desired
    result.                                 ]#
import strutils
import parser
import tables
import strformat
from random import rand

type
    #These are the two types any expression could evaluate into.
    # will be used to see what operations are being done on.
    # CErr is returned to denote an error
    CheckValue* = enum CList, CInt, CErr

    #Every type check function returns a report like this, which will have 
    #some returning type, a flag to denote error, and possible error string.
    CheckReport* = object 
        value*: CheckValue
        error*: bool
        errorStr*: string
    
    #The interpreter which will go through the AST of the expression
    # and try to return a final value. Can have an error.
    Interpreter* = object
        ast*: Node
        error*: bool
        errorStr*: string
    
    EValType* = enum
        EErr
        EInt
        EList
    
    EValue* = ref object of RootObj
        case evalType*: EValType:
        of EErr:
            estr*: string
        of EInt:
            eint*: int
        of EList:
            elist*: seq[EValue]



#=========[  TYPE CHECKER  ]=========#

#Creates a new instance of a check report
func newCheckReport(value: CheckValue, error: bool, errorStr: string): CheckReport =
    CheckReport(value: value, error: error, errorStr: errorStr)

#If the last report has an error, don't test this, 
# return this last report to tell user that error.
template checkReport(report: CheckReport) =
    if report.error:
        return report

#Forward declare this function for easy recursive calls.
proc check*(statement: Node, env:Table[string, EValue]): CheckReport 

#Checking a number is one of the few guarenteed things
proc checkNumber(integer: Node): CheckReport =
    newCheckReport(CInt, false, "")

#Checks each value in a list
proc checkList(list: Node, env:Table[string, EValue]): CheckReport =
    for value in (List list).values:
        let elemReport = check(value, env) #Get report on this expression
        elemReport.checkReport()      #Check if there is an error to give

        #if there is a nested list within this list, raise an error
        if elemReport.value == CList:
            return newCheckReport(CErr, true, "Type error, cannot have a list within a list.")
    
    #Oherwise, perfectly legal list, return the list
    newCheckReport(CList, false, "")

#For the unary operation, it just checks it's node
proc checkUnaryOp(unaryOp: Node, env: Table[string, EValue]): CheckReport =
    let 
        #Check the type of the value
        operand = check((UnaryOp unaryOp).value, env)
        #Get the operator that is acting upon the value
        operator = (UnaryOp unaryOp).operator

    #Check if the value has some sort of error
    operand.checkReport()

    #If this is advantage or disadvantage.. always returns an integer
    if operator == Disadvantage or operator == Advantage:
        return newCheckReport(CInt, false, "")

    #Otherwise, just returns same type
    return operand

#Checks types of operands to see if there is a type error
proc checkBinOp(binOp: Node, env: Table[string, EValue]): CheckReport =
    let #Get operator and left side value
        op   = (BinOp binOp).operator
        left = check((BinOp binOp).left, env)

    left.checkReport() #Check report from left value for error

    #Get the right value, and check it for an error
    let right = check((BinOp binOp).right, env)
    right.checkReport()

    #If the left value is a number, right is list, op is div, raise error
    if left.value == CInt and right.value == CList and op == Div or op == Mod:
        return newCheckReport(CErr, true, "Type error, cannot divide a number by a list.")

    #[ Not returning errors ]#

    #If either value is a list, then the final value is a list
    if left.value == CList or right.value == CList:
        return newCheckReport(CList, false, "")
    
    #Otherwise, the returned value is an integer
    else:
        return newCheckReport(CInt, false, "")

#Checks out types for dice.
proc checkDice(dice: Node, env: Table[string, EValue]): CheckReport =
    let 
        diceType = (Dice dice).operator
        times    = (Dice dice).times

    #If this is a sum roll, return a CInt
    if diceType == SumRoll:
        return newCheckReport(CInt, false, "")

    #If this is a dice roll, and the dice is being rolled once, will be an integer
    elif diceType == DiceRoll and times of Number and (Number times).value == 1:
        return newCheckReport(CInt, false, "")

    else: #The dice roll will result in a list
        return newCheckReport(CList, false, "")

#This will go through all the nodes in the parsed sub expressions
# to find out if the types check out for this.
proc check(statement: Node, env: Table[string, EValue]): CheckReport =
    #Find out what this node is, and check it out
    if statement of Number:
        result = checkNumber(statement)
    
    elif statement of List:
        result = checkList(statement, env)
    
    elif statement of Dice:
        result = checkDice(statement, env)
    
    elif statement of UnaryOp:
        result = checkUnaryOp(statement, env)
    
    elif statement of BinOp:
        result = checkBinOp(statement, env)
    
    #Just check the value of the assignment
    elif statement of Assignment:
        return check((Assignment statement).value, env)
    
    elif statement of VariableRecall:
        #If this variable exists in environment, pull the type info of its value
        if (VariableRecall statement).name in env:
            let val: EValue = env[(VariableRecall statement).name]

            case val.evalType:
                of EInt:
                    return newCheckReport(CInt, false, "")
                of EList:
                    return newCheckReport(CList, false, "")
                of EErr:
                    return newCheckReport(CErr, true, "Undef Error")
            
        #Else, variable doesn't exist, complain
        else:
            return newCheckReport(CErr, true, fmt"Variable '{(VariableRecall statement).name}' is not declared.")


#=========[  INTERPRETER  ]=========#

#Creates a new instance of the interpreter
proc newInterpreter*(text: string, env: Table[string, EValue]): Interpreter =
    var parser = newParser(text) #Creates the parser to be used
    let ast    = parser.parse()  #Attempt to parse the text
    var #Make the error, errorStr, may be changed to any sub errors
        error  = false
        errorStr = ""
    
    #Get a report on how it is
    let report = check(ast, env)

    #Before checking type errors, if there are any
    # parsing errors, interpreter adopts that error
    if parser.error:
        error = true
        errorStr = parser.errorStr

    #If the report has an error, make the
    # interpreter adopt the error aswell.
    elif report.error:
        error = true
        errorStr = report.errorStr

    #Creates the instance of the interpreter.
    Interpreter(ast: ast, error: error, errorStr: errorStr)

#Return a new instance of an error
func newEErr(error: string): EValue = EValue(evalType:EErr, estr: error)
#Makes a new instance of a return integer
func newEInt*(value: int): EValue = EValue(evalType:EInt, eint: value)
#Makes a new instance of a return list
func newEList*(values: seq[EValue]): EValue = EValue(evalType:EList, elist: values)

#Extracts the integer value from a EInt
func getEInt*(val: EValue): int = val.eint
#Extracts the list from teh elist
func getEList*(val: EValue): seq[EValue] = val.elist
#Get the error string from the value
func getEErr*(val: EValue): string = val.estr


#=========[  STRING CONVERSION  ]=========#

proc `$`*(val: EValue): string =
    if val.evalType == EInt:
        return $getEInt(val)

    elif val.evalType == EList:
        var strings: seq[string] #All values's strings

        #Go through each value
        for elem in getEList(val):
            strings.add( $elem )
        
        #Returns the string
        return "[" & strings.join(" ") & "]"


#==========[ EVAL FUNCTIONS ]==========#

#Predeclare this function so we can have RECURSION
proc eval*(node: Node, lastAnswer: EValue, env:var Table[string, EValue]): EValue 

#Checks if an error occured, if it did, pass it on.
template checkError(value: EValue) =
    if value.evalType == EErr:
        return value

#Evaluates a list of values.
proc evalList(node: Node, lastAnswer: EValue, env:var Table[string, EValue]): EValue = 
    #All of the evaluated return values
    var evaluatedVals: seq[EValue] = @[]

    #Go for each value and evaliate each one
    for value in (List node).values:
        #Evaluate this element in this list
        let evaluatedVal = eval(value, lastAnswer, env)
        #Check if there was an error in evaluation
        evaluatedVal.checkError()
        #Add this evaluated value to the list
        evaluatedVals.add(evaluatedVal)

    #Return the return list of evaluted values.
    newEList(evaluatedVals)

#The number of maximums and minimums
template naturalMaxMinCount(sides: int, list: seq[EValue]) =
    #If there are multiple ones, tell user the count
    if countin(1, list) > 0:
        echo "- Natural 1! x " & $countin(1, list)
    
    #If there are multiple maximums, tell user the count
    if countin(sides, list) > 0:
        echo "- Natural " & $sides & "! x " & $countin(sides, list)

#Rolls a single dice.
proc roll(sides: int): int =
    if sides > 1:
        return rand(1.. sides)
    elif sides == 1:
        return 1
    elif sides == 0:
        return 0

#Count the number of occurance of `item` in `vals`
proc countin(item:int, vals: seq[EValue]): int =
    var count = 0
    
    for value in vals:
        if getEInt(value) == item:
            count += 1
    
    return count

#Evaluate the dice and return its' value
proc evalDice(node: Node, lastAnswer: EValue, env:var Table[string, EValue]): EValue =
    let 
        #The number of times the dice should be rolled.
        timesNode = eval((Dice node).times, lastAnswer, env)
        #The number of sides this dice has.
        sidesNode = eval((Dice node).sides, lastAnswer, env)
    
    #Check if there are any errors for times and sides
    timesNode.checkError()
    sidesNode.checkError()

    let #Get the times and sides from the nodes
        times = getEInt(timesNode)
        sides = getEInt(sidesNode)
    
    #If there is only supposed to be one value, return that number
    if times == 1:
        let diceRoll = roll(sides)

        #If user rolled a one, tell them it was a natural one
        if diceRoll == 1:
            echo "- Natural 1!"
        
        #If the user rolled a maximum, tell the user
        elif diceRoll == sides:
            echo "- Natural " & $sides & "!"
        
        return newEInt(diceROll)

    elif times > 0: #Otherwise, return the list        
        var list: seq[EValue] #List of all the values

        #Make the list `times` long
        while len(list) != times:
            list.add newEInt(roll(sides))
        
        #Report on number of maximum and minimums found
        naturalMaxMinCount(sides, list)

        return newEList(list)

#Evaluates a unary operator, be it sum or negation
proc evalUnaryOp(node: Node, lastAnswer: EValue, env:var Table[string, EValue]): EValue =
    let #Get the operator & value operating on
        operator = (UnaryOp node).operator
        value    = eval((UnaryOp node).value, lastAnswer, env)
    
    #Check if there was an error with evaluating the value
    value.checkError()

    #Negate the number given
    if operator == Sub and value.evalType == EInt:
        return newEInt(- getEInt(value))
    
    #If this is negation on list.. negate every value
    elif operator == Sub and value.evalType == EList:
        var newList: seq[EValue] #Evaluated negated values
        var item: EValue

        for element in getEList(value):
            item.checkError()    #Check if there is an error with this element
            #Negate the value and add it to the new list of values
            newList.add(newEInt(- getEInt(item)))
        
        #Return the new list of values
        return newEList(newList)

    #If we are taking sum of a list, add all the values together
    elif operator == Sum and value.evalType == EList:
        var sum = 0 #The sum from the list

        for element in getEList(value):
            element.checkError()       #Check for an error
            sum += getEInt(element)    #Add this value to the sum
        
        #Return the completed sum
        return newEInt(sum)
    
    #If this is the sum of an integer, it is just that integer
    elif operator == Sum and value.evalType == EInt:
        return value
    
    #Find the largest value within the list and return it
    elif operator == Advantage and value.evalType == EList:
        #If this list has no values in it, return zero.
        if len(getEList(value)) == 0:
            return newEInt(0)

        else: #Get the maximum of all the values
            var 
                values   = getEList(value)
                largest = getEInt(values[0])
            
            #Go through, find the largest value, return it
            for item in values:
                let suggestion = getEInt(item)

                if suggestion > largest:
                    largest = suggestion
            
            echo "- Advantage: " & $largest
            
            return newEInt(largest)

    #Find the smallest value within the list and return it
    elif operator == Disadvantage and value.evalType == EList:
        #If this list has no values in it, return zero.
        if len(getEList(value)) == 0:
            return newEInt(0)

        else: #Get the maximum of all the values
            var 
                values   = getEList(value)
                smallest = getEInt(values[0])
            
            #Go through, find the largest value, return it
            for item in values:
                let suggestion = getEInt(item)

                if suggestion < smallest:
                    smallest = suggestion
            
            echo "- Disadvantage: " & $smallest
            
            return newEInt(smallest)

    #If this value is an integer, evaluate again, get higher
    elif operator == Advantage and value.evalType == EInt:
        let #The first calculated roll from this node
            firstRoll = getEInt(value)
            #Take another shot at calculating a different value.
            secondRoll = getEInt(eval((UnaryOp node).value, lastAnswer, env))

        #If the two values are the same, return first
        if firstRoll == secondRoll:
            echo "- Advantage: " & $firstRoll

            return newEInt(firstRoll)

        else:
            #get the maximum out of the two values
            let maximum = max(firstRoll, secondRoll)

            echo "- Advantage: " & $maximum

            return newEInt(maximum)

    #If this value is an integer, evaluate again, get lower 
    elif operator == Disadvantage and value.evalType == EInt:
        let #The first calculated roll from this node
            firstRoll = getEInt(value)
            #Take another shot at calculating a different value.
            secondRoll = getEInt(eval((UnaryOp node).value, lastAnswer, env))

        #If the two values are the same, return first
        if firstRoll == secondRoll:
            echo "- Disadvantage: " & $firstRoll

            return newEInt(firstRoll)

        else:
            #get the maximum out of the two values
            let minimum = min(firstRoll, secondRoll)

            echo "- Disadvantage: " & $minimum

            return newEInt(minimum)

#Evaluates the binary operation if both are integer values
proc evalBinBothInt(operator: TokenType, left, right, lastAnswer: EValue): EValue = 
    let #Get the integer values for both sides
        x = getEInt(left)
        y = getEInt(right)
    
    #If Addition, add the two values, store result
    if operator == Add:
        result = newEInt(x + y)
    
    #If Subtraction, take difference, return result
    elif operator == Sub:
        result = newEInt(x - y)
    
    #Multiply the two values
    elif operator == Mult:
        result = newEInt(x * y)

    #Divides the two values, save result
    elif operator == Div and y != 0:
        return newEInt(x div y)

    #Get x mod y
    elif operator == Mod and y != 0:
        return newEInt(x mod y)
    
    #If the right side value is zero, raise error
    elif operator == Div or operator == Mod and y == 0:
        return newEErr("Error, cannot do division by zero.")

#Evaluates teh binary operation if both are lists
proc evalBinBothList(operator: TokenType, left, right, lastAnswer: EValue): EValue = 
    var
        x = getEList(left)
        y = getEList(right)
        newList: seq[EValue]
    
    #If x is greater than y, make y same size as x
    while len(x) > len(y): y.add(newEInt(0))
    #If y is greater than x, make x same size as y
    while len(y) > len(x): x.add(newEInt(0))

    if operator == Add:
        for i in 0..<len(x):
            let #Get the integers from the lsit
                xi = getEInt(x[i])
                yi = getEInt(y[i])

            newList.add(newEInt(xi + yi))
        
        return newEList(newList)
    
    elif operator == Sub:
        for i in 0..<len(x):
            let
                xi = getEInt(x[i])
                yi = getEInt(y[i])
            
            newList.add(newEInt(xi - yi))
        
        return newEList(newList)

    elif operator == Mult:
        for i in 0..<len(x):
            let
                xi = getEInt(x[i])
                yi = getEInt(y[i])
            
            newList.add(newEInt(xi * yi))
        
        return newEList(newList)

    elif operator == Div and countin(0, y) == 0:
        for i in 0..<len(x):
            let
                xi = getEInt(x[i])
                yi = getEInt(y[i])
            
            newList.add(newEInt(xi div yi))
        
        return newEList(newList)

    elif operator == Mod and countin(0, y) == 0:
        for i in 0..<len(x):
            let
                xi = getEInt(x[i])
                yi = getEInt(y[i])
            
            newList.add(newEInt(xi mod yi))
        
        return newEList(newList)

    elif operator == Div or operator == Mod and countin(0, y) > 0:
        return newEErr("Error, cannot do division by zero.")

#EValuates w/ all operations when left value is an int and right is a list
proc evalBinLeftInt(operator: TokenType, left: int, right: seq[EValue], lastAnswer: EValue): EValue =
    var newList: seq[EValue]

    if operator == Add:
        for i in 0..<len(right):
            let yi = getEInt(right[i])

            newList.add(newEInt(left + yi))

        return newEList(newList)

    elif operator == Sub:
        for i in 0..<len(right):
            let yi = getEInt(right[i])

            newList.add(newEInt(left - yi))

        return newEList(newList)
    
    elif operator == Mult and left == 0:
        return newEInt(0)

    elif operator == Mult:
        for i in 0..<len(right):
            let yi = getEInt(right[i])

            newList.add(newEInt(left * yi))

        return newEList(newList)

    elif operator == Div or operator == Mod and countin(0, right) > 0:
        echo "This is the one!"
        return newEErr("Error, cannot do division by zero.")

    elif operator == Div:
        for i in 0..<len(right):
            let yi = getEInt(right[i])

            newList.add(newEInt(left div yi))

        return newEList(newList)
    
    elif operator == Mod:
        for i in 0..<len(right):
            let yi = getEInt(right[i])

            newList.add(newEInt(left mod yi))

        return newEList(newList)

#Evaluate all of the operations when the left value is a list, and right is an int
proc evalBinLeftList(operator: TokenType, left: seq[EValue], right: int, lastAnswer: EValue): EValue =
    #If subtraction, negate the number, add the negative value to list
    if operator == Sub:
        return evalBinLeftInt(Add, -right, left, lastAnswer)

    #Divide each element by this constant
    elif operator == Div and right != 0:
        var newList: seq[EValue]

        for i in 0..<len(left):
            let xi = getEInt(left[i])

            newList.add(newEInt(xi div right))
        
        return newEList(newList)
    
    #Perform mod on each element on this list by this int
    elif operator == Mod and right != 0:
        var newList: seq[EValue]

        for i in 0..<len(left):
            let xi = getEInt(left[i])

            newList.add(newEInt(xi mod right))
        
        return newEList(newList)

    #Catch zero division errors
    elif operator == Div or operator == Mod and right == 0:
        return newEErr("Error, cannot do division by zero.")
    

    #For all communitive operations, just flip sides, use other function
    else:
        return evalBinLeftInt(operator, right, left, lastAnswer)

#Evaluates some sort of binary operation.
proc evalBinOp(node: Node, lastAnswer: EValue, env:var Table[string, EValue]): EValue =
    let
        operator  = (BinOp node).operator
        left      = eval((BinOp node).left, lastAnswer, env)
        right     = eval((BinOp node).right, lastAnswer, env)
        leftType  = left.evalType
        rightType = right.evalType
    
    left.checkError()
    right.checkError()

    #If both values are integers, evaluate them w/ this operator
    if leftType == EInt and rightType == EInt:
        return evalBinBothInt(operator, left, right, lastAnswer)

    #If both values are lists, evaluate this operation on them
    elif leftType == EList and rightType == EList:
        return evalBinBothList(operator, left, right, lastAnswer)
    
    elif leftType == EList and rightType == EInt:
        return evalBinLeftList(operator, getEList(left), getEInt(right), lastAnswer)

    else:
        return evalBinLeftInt(operator, getEInt(left), getEList(right), lastAnswer)

#Evaluates any node, figures out what kind of 
# node this is and then evaluates it accordingly
proc eval*(node: Node, lastAnswer: EValue, env:var Table[string, EValue]): EValue =
    #If the value is just a number, return the number.
    if node of Number:
        return newEInt((Number node).value)

    #Pull out the value of the variable
    elif node of VariableRecall:
        let name = (VariableRecall node).name

        if name notin env:
            return newEErr(fmt"Undefined variable, {name}")

        #Pull out the value of this variable
        let value: EValue = env[name]
        
        #Return the value
        return value

    #Stores value in a variable
    elif node of Assignment:
        let node = (Assignment node)
        
        #If this term is not a variable recall, then this is malformed.
        if not (node.variable of VariableRecall):
            return newEErr("Malformed assignment operator! First term cannot be an expression.\nUse form VARIABLE = EXPRESSION")

        let 
            name = (VariableRecall node.variable).name
            value = eval(node.value, lastAnswer, env)
        
        #Ensure there is no error
        value.checkError() 

        #Store value of variable in environment
        env[name] = value

        return value

    #If this is a list, try to evalutate it.
    elif node of List:
        return evalList(node, lastAnswer, env)
    
    #If this is a dice, evaluate it.
    elif node of Dice:
        return evalDice(node, lastAnswer, env)

    #If this is a unary operation, evaluate it
    elif node of UnaryOp:
        return evalUnaryOp(node, lastAnswer, env)
    
    #If this is some binary operation, evaluate it.
    elif node of BinOp:
        return evalBinOp(node, lastAnswer, env)

    #If this node is the previously computed answer, return it
    elif node of LastAnswer:
        return lastAnswer


