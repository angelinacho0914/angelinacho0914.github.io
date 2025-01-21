---
title: "Simple Battleship Simulation" 
date: 2022-05-01
lastmod: 2022-05-01
tags: ["pygame","python"]
author: ["Angelina Cho","K Yabe"]
description: "This project is for OIM3640 at Babson College, taught by Professor Zhi Li." 
summary: "Althogh it may look like a relatively simple game, battleship game actually requires many important concepts from object-oriented programming such as class, object, abstraction, and encapsulation. Through this project, our team has tried to crystallize our learning throughout this semester into something tangible." 
cover:
    image: "battleship.png"
    alt: "Simple Battleship Simulation"
    relative: false

---

##### Introduction

This project is for OIM3640 at Babson College, taught by Professor Zhi Li.

As our team members comtemplated potential ideas for our project, we have come to a conclusion to work on one that will help us review our learning from our class and require a decent amount of self-learning. We came up with this idea of battleship game because it challenges us to incorporate significant concepts from object-oriented programming. We figured that this project will help us start with the minimum requirement and develop further if we have great ideas.

---

##### Basic Specs

Our battleship is deployed on 10 x 10 matrices against CPU. On the screen, a player will see 3 matrices: his own ship positions in the left-bottom one, his attack on the left-top one, and CPU's attack on the right-bottom one. For a shot without hit, a pink point will show up on the grid. Once you hit a part of a ship's body, the point will show up as orange, and the orange points will turn to red once the player completely destroys the ship.

---

##### Technical Architecture (back-end)

Our battleship game consists of 2 files: battleship.py and engine.py. battleship.py takes the role of front-end, returning the user interface. In contract, engine.py powers battleship.py from the back-end, supporting the implementation using object-oriented programming.

engine.py is made of 3 classes of objects: Ship, Player, and Game. Each class has initial attributes, describing the functions of each class. For example, Ship class is initialized with the a beginning position of a ship, and afterwards spreads in one direction so that a ship stays consistent. Player is initialized as placing those ships onto the grid, and Game works as a CPU playing the player.

---

##### Technical Architecture (front-end)

Explaining the functionality of battleship.py

Our battleship.py makes use of global variables so as to fix the underlying game scaffolding intact throughout different functions. Functions are used to draw grids, player's ship distribution, and CPU's ship distrition, both of which are randomly chosen. Afterwards, the overall file will track user interaction throught a boolean object "animating." Using while loop, the file will call upon pygame to communicate user responses and CPU's attack while "animating" is true. As soon as "animating" becomes false, the while loop quits pygame and shows the victorious message for a designated player.

---

##### Instructions

To get started, download the codes from the related material below. Using "pip install" (i.g. pip install --pygame) in your terminal or command window, please install numpy, matlibplot, and pygame onto your device.

---

##### Related material

+ [Download codes here](https://github.com/kyabe314/oim3640_finalproject/tree/main/code)
+ [README (GitHub)](https://github.com/kyabe314/oim3640_finalproject/blob/main/README.md)
