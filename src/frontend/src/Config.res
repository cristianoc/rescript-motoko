let randomSeed = (~level as _) => 34

let levelWidth = (~level) =>
  switch level {
  | 1 => 800.
  | 2 => 1200.
  | 3 => 2400.
  | 4 => 3500.
  | 5 => 4500.
  | 6 => 6000.
  | 7 => 8000.
  | 8 => 10000.
  | 9 => 12000.
  | _ => 1500. *. float_of_int(level)
  }

let levelHeight = (~level as _) => 256.

let enemyDensity = (~level) =>
  switch level {
  | 1 | 2 | 3 => 20 // One out of 20 blocks has an enemy on it
  | 4 | 5 => 15
  | 6 => 10
  | 7 | 8 => 5
  | 9 => 4
  | _ => 3
  }

let canvasId = "canvas"

let delayWhenFinished = 300.

let images = ["blocks.png", "items.png", "enemies.png", "mario-small.png"]

let blockw = (~level) => levelWidth(~level) /. 16.
let blockh = (~level) => levelHeight(~level) /. 16. -. 1.

let mapDim = (~level) => (levelWidth(~level), levelHeight(~level))

let restartAfter = 5.

let spritesDir = "sprites/"

let scale = 1.5

let friction = 0.9

let gravity = 0.2

let maxYVel = 4.5

let playerSpeed = 2.8

let playerJump = 5.7

let playerMaxJump = -6.

let dampenJump = 4.

let invuln = 60
