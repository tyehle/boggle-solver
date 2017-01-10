# README #

This is a project to solve a game of boggle. It is configured to be easy to generate boards using the dice commonly found in available copies of the game.

### Design ###

The state of the game has two parts, the game board, and the list of valid words. The list of valid words are stored as a tree to make searching for them in the board fast.

### Building the Project ###

* Scala version 2.12
* Build using [sbt](http://www.scala-sbt.org/)
* No external dependencies
