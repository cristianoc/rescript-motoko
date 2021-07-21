let new = (~level, ~score) => {
  let player1 = One->Generator.newPlayer(~level)
  let player2 = Two->Generator.newPlayer(~level)
  let viewport = Viewport.make(~level)
  viewport->Viewport.update(player1.px, player1.py)
  let objects = Generator.generate(~level)
  {
    Types.bgd: Sprite.makeBgd(),
    coins: 0,
    level: level,
    multiplier: 1,
    objects: objects,
    particles: [],
    player1: player1,
    player2: player2,
    score: score,
    viewport: viewport,
  }
}

// Add [i] to the score in [state]
let updateScore = (state: Types.state, i) => state.score = state.score + i
