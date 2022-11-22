import parser
import interpreter
import terminal

type
    Mode = enum
        Macro
        Normal
        Insert
    
    Line = tuple[linenum: int, text: string]

const 
    LINE_START = 4

proc editor*: seq[string] =
    stdout.eraseScreen()
    stdout.setCursorPos(4,0)

    let
        height = terminalHeight()

    var 
        memory: seq[Line]
        posYStack: seq[int] = @[]
        line = ""
        mode = Normal 
        key = '\0'
        yPos = 0
        xPos = LINE_START
        halt = false
        done = false
    
    for i in 1..(height-1):
        memory.add (linenum: i, text: " ")
    

    stdout.setCursorXPos(0)
    for line in memory:
        stdout.write line.linenum

        if line.linenum < 10:
            stdout.write "  "
        
        else:
            stdout.write " "
            
        stdout.writeLine line.text 
    
    stdout.setCursorXPos(LINE_START)
    
    while not done:
        var buff = 0

        if mode == Insert:
            buff = 4
        
        elif mode == Macro:
            buff = 2

        key = getch()
        
        #If in a mode where typing effects text, increment xpos
        if mode != Normal:
            xpos += 1

        #Enter into macro mode
        if key == ':' and mode == Normal:
            mode = Macro
            line = ""
            posYStack.add yPos 
            yPos = height-1
            xPos = 0
            stdout.setCursorPos(xPos, yPos)
            stdout.write ':'
            halt = true
        
        elif key == '\b' and mode != Normal and 
            line = line[0..^2]
            xPos -= 1
            stdout.setCursorXPos(buff+xPos-2)
            stdout.write ' '
            stdout.setCursorXPos(buff+xPos-1)
        
        elif mode == Macro and key == '\r' or mode == Macro and line == "" and key == '\b':
            stdout.eraseLine()

            stdout.setCursorPos(15, 0)
            stdout.write line

            for command in line:
                if command == 'q':
                    done = true
                    break

            yPos = posYStack.pop()
            line = memory[yPos].text
            mode = Normal
            stdout.setCursorPos(LINE_START, yPos)
            halt = true

        
        if not halt:
            stdout.write key
            line.add key

        halt = false
    
    stdout.eraseScreen()
    stdout.setCursorPos(0,0)

