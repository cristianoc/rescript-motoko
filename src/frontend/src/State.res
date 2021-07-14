type status =
  | Loading
  | Paused
  | Playing
  | Finished({levelResult: Actors.levelResult, restartTime: float})
  | Saving

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
  level: int,
  mutable multiplier: int,
  mutable objects: list<Object.t>,
  mutable particles: list<Particle.t>,
  player1: Object.t,
  player2: Object.t,
  mutable score: int,
  mutable status: status,
  viewport: Viewport.t,
}

let new = (~level) => {
  let player1 = One->Generator.newPlayer(~level)
  let player2 = Two->Generator.newPlayer(~level)
  let viewport = Viewport.make(~level)
  viewport->Viewport.update(player1.px, player1.py)
  let objects = Generator.generate(~level)
  {
    bgd: Sprite.makeBgd(),
    coins: 0,
    level: level,
    multiplier: 1,
    objects: objects,
    particles: list{},
    player1: player1,
    player2: player2,
    score: 0,
    status: Playing,
    viewport: viewport,
  }
}

// Add [i] to the score in [state]
let updateScore = (state, i) => state.score = state.score + i

let current = ref(new(~level=1))

let load = () => {
  Backend.service.loadGameState()->Promise.then(json => {
    current := json->Js.Json.parseExn->Obj.magic
    Promise.resolve()
  })
}

let save = () => Backend.service.saveGameState(current.contents->Obj.magic->Js.Json.stringify)
