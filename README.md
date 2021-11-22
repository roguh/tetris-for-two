# Tetris for two
A fun tetris game with many different game modes.

## PLAY HERE: [roguh.com/projects/tetris-for-two](https://roguh.com/projects/tetris-for-two/)

## Player 1 (left) controls:

Press these `BUTTONS` to move your tetromino and beat your opponent!

**Move tetromino to the left**: `A` 

**Move tetromino to the right**: `D` 

**Drop tetromino faster**: `S`

**Drop tetromino immediately**: `W`

**Rotate tetromino to the left**: `Z`

**Rotate tetromino to the right**: `X`

**Save piece for later**: `T`


## Player 2 (right) controls:

Press these `BUTTONS` to move your tetromino and beat your opponent!

**Move tetromino to the left**: `left arrow`  

**Move tetromino to the right**: `right arrow`  

**Drop tetromino faster**: `down arrow` 

**Drop tetromino immediately**: `up arrow` 

**Rotate tetromino to the left**: `,` 

**Rotate tetromino to the right**: `.` 

**Save piece for later**: `L` 

## Development

### With ASDF

This project needs an older version of Elm, so an elm version manager is recommended.

Here are instructions using ASDF:

```
$ asdf plugin add elm
$ asdf install elm 0.18.0
$ asdf shell elm 0.18.0
```

### Building

Make sure you have Elm version 0.18.0 or up and less than 0.19.0:

```
$ elm --version
0.18.0
```

Build by running:

```
./build.sh
```

Output will be in `./main.js`.
