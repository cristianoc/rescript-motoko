type status =
  | Playing
  | Finished({levelResult: Actors.levelResult, finishTime: float})

// State of the game. It includes a background sprite (e.g.,
// (e.g., hills), a context (used for rendering onto the page), a viewport
// (used for moving the player's "camera"), a score (which is kept track
// throughout the game), coins (also kept track through the game),
// a multiplier (used for when you kill multiple enemies before ever touching
// the ground, as in the actual Super Mario), and a game_over bool (which
// is only true when the game is over).
type t = {
  bgd: Sprite.t,
  mutable coins: int,
  mutable level: int,
  mutable multiplier: int,
  mutable score: int,
  mutable status: status,
  viewport: Viewport.t,
}

let new = (~level, ~viewport) => {
  bgd: Sprite.makeBgd(),
  coins: 0,
  level: level,
  multiplier: 1,
  score: 0,
  status: Playing,
  viewport: viewport,
}

// Add [i] to the score in [state]
let updateScore = (state, i) => state.score = state.score + i
