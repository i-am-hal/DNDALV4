#[  Alastar Slater
    April 15, 2021
    repl.nim

    Contains the REPL, error checking/displying
    code, and all of the code needed to display
    the result from some computation.            ]#
import interpreter, parser, random, strutils, strformat
import terminal

#Clears out a line of text
proc clearLine(minValue, length: int) =
    stdout.setCursorXPos(minValue - 1)
    var i = 0

    while i < length:
        stdout.write ' '
        i += 1

#Displays a string for us.
proc displayString(minValue: int, xpos: var int, str: string) =
    xpos = 0
    stdout.setCursorXPos(minValue - 1)

    for chr in str:
        stdout.write chr
        xpos += 1

#Deals with getting input from the user, allows
# user to use the arrow keys.
proc getInput(history: var seq[string], historyIndex: var int, breakLoop: var bool): string =
    let 
        minXValue = 3
        deleteChars = [char(127), char(126)]

    var 
        chr = '\0'
        line: string
        xpos = 0

    #Print out the prompt on this line
    stdout.write("> ")

    while chr notin "\n\r":
        chr = getch()

        #If this is a sequence about moving the cursor
        if chr == '\x1b':
            discard getch()
            #What direction to move cursor
            let arrowKey = getch()

            #Do nothing for left/right
            if arrowKey in "CD":
                discard 0

            #Going up through history
            elif arrowKey == 'A' and historyIndex+1 < len(history):
                historyIndex += 1
                clearLine(minXValue, len(line))
                line = history[historyIndex]
                displayString(minXValue, xpos, line)
            
            #Going down in history to most recently typed thing
            elif arrowKey == 'B' and historyIndex > 0:
                historyIndex -= 1
                clearLine(minXValue, len(line))
                line = history[historyIndex]
                displayString(minXValue, xpos, line)
        
        elif xpos + minXValue != minXValue and chr in deleteChars:
            line = line[0..^2]
            stdout.cursorBackward()
            stdout.write ' '
            stdout.cursorBackward()
            xpos -= 1
        
        #Break out of the program prematurely
        elif chr == char(26):
            breakLoop = true
            break
            
        #Otherwise, normal char, then add it to the string
        elif chr notin deleteChars:
            line.add chr
            stdout.write chr
            xpos += 1
    
    #Add new line after this line of input
    stdout.write '\n'

    return line.strip()

proc repl =
    stdout.eraseScreen()
    stdout.setCursorPos(0,0)
    randomize()
    echo "D&DAL - Dungeons and Dragons Automation Language"
    echo "Enter Ctrl+Z or 'quit' to quit.\n"

    var 
        #The last computed result
        lastResult: EValue = newEInt(0)
        #History of all previously typed computations
        history: seq[string] = @[""]
        #Break out of the program from ctrl+z
        breakLoop = false

    while true:
        #Index into history list
        var historyIndex = 0

        let 
            input = getInput(history, historyIndex, breakLoop)
            words = input.strip().toUpperAscii().split()
        
        #If the user wants to exit the program, and they typed quit, quit
        if len(words) > 0 and words[0] == "QUIT" or breakLoop:
            break

        #If this line is a comment, get input again
        elif len(input) > 1 and input[0] == ';':
            stdout.write '\n'
            continue

        #Add this given line to the history of computations if last thing wasn't the same
        if len(history) > 2 and history[1] != input:
            history.insert(input, 1)
        else:
            history.insert(input, 1)
            
        var 
            parser = newParser(input)      #Make the parser which will parse all the input
            ast    = parser.parse()        #Parse the given input given to us
            report = check(ast)            #Get a report on if there are any type errors
            result = eval(ast, lastResult) #Evaluate the expression, see if any runetime errors occured
        
        #If the parser had some sort of error, report it
        if parser.error:
            echo fmt"{parser.errorStr}"
            stdout.write "\n"
        
        #If the checking of the types failed, report that
        elif report.error:
            echo fmt"{report.errorStr}"
            stdout.write "\n"
        
        #If the result is an error, report whatever that was
        elif result.evalType == EErr:
            echo fmt"{getEErr(result)}"
            stdout.write "\n"
        
        else:
            #Save this computation
            lastResult = result
            #Print out the result
            echo result, "\n"
            

#If this is the main function for all of this
if isMainModule:
    repl()