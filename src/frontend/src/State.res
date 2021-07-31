let new = (~date, ~level, ~score) => {
  let viewport = Viewport.make(~level)
  let incompleteState = {
    Types.bgd: Sprite.makeBgd(),
    coins: 0,
    date: date,
    idCounter: 0,
    level: level,
    multiplier: 1,
    objects: [],
    particles: [],
    player1: Obj.magic(0), // don't do this at home
    player2: Obj.magic(0), // don't do this at home
    score: score,
    viewport: viewport,
  }
  let player1 = One->Generator.newPlayer(~state=incompleteState)
  let player2 = Two->Generator.newPlayer(~state=incompleteState)
  viewport->Viewport.update(player1.px, player1.py)
  Generator.generate(~state=incompleteState)
  {...incompleteState, player1: player1, player2: player2}
}

// Add [i] to the score in [state]
let updateScore = (state: Types.state, i) => state.score = state.score + i
