# RushHour 
This project implements a solution to the game Rush Hour([desciption](https://en.wikipedia.org/wiki/Rush_Hour_(puzzle))
using an A Star search 

## Usage
We represent the board using the State type.The user provides the data as a string with with each row seperated by a
newline and the colors represented by ANSI characters.The "red" car is represented by the '=' character and the empty cell
by the '.' character.
eg.
aa..b
ccc.b => "aa..b\nccc.b\n==..b\n.dddb"
==..b
.dddb
