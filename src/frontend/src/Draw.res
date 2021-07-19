open Belt
open Sprite

let renderBbox = (sprite, posx, posy) => {
  let (bbox, bboy) = sprite.params.bboxOffset
  let (bbsx, bbsy) = sprite.params.bboxSize
  let context = Load.getContext()
  context.strokeStyle = "#FF0000"
  context.strokeRect(. posx +. bbox, posy +. bboy, bbsx, bbsy)
}

// Draws a sprite onto the canvas
let render = (sprite, posx, posy) => {
  let (sx, sy) = sprite.params.srcOffset
  let (sw, sh) = sprite.params.frameSize
  let (dx, dy) = (posx, posy)
  let (dw, dh) = sprite.params.frameSize
  let sx = sx +. float_of_int(sprite.frame) *. sw
  let context = Load.getContext()
  context.drawImage(. sprite.params.png->Sprite.Png.toImg, sx, sy, sw, sh, dx, dy, dw, dh)
}

// Draws two background images, which needs to be done because of the
// constantly changing viewport, which is always at most going to be
// between two background images.
let drawBgd = (bgd, off_x) => {
  render(bgd, -.off_x, 0.)
  render(bgd, fst(bgd.params.frameSize) -. off_x, 0.)
}

/* Parallax background */
let drawBgd = (state: State.t) => {
  let vposXInt = int_of_float(state.viewport.px /. 5.)
  let bgdWidth = int_of_float(fst(state.bgd.params.frameSize))
  let off_x = @doesNotRaise float_of_int(mod(vposXInt, bgdWidth))
  drawBgd(state.bgd, off_x)
}

// Used for animation updating. Canvas is cleared each frame and redrawn.
let clearCanvas = () => {
  let {sizeScaled: {widthScaled, heightScaled}} = Load.getCanvasData()
  Load.getContext().clearRect(. 0., 0., widthScaled, heightScaled)
}

let centerXText = (txt, ~fontSize, ~y) => {
  let ctx = Load.getContext()
  let {sizeScaled: {widthScaled}} = Load.getCanvasData()
  let fontTxt = fontSize->int_of_float->string_of_int ++ "px"
  ctx.font = fontTxt ++ "'Press Start 2P'"
  let xCentered = (widthScaled -. fontSize *. float_of_int(String.length(txt))) /. 2.
  ctx.fillText(. txt, xCentered, y)
}

let centerXYText = (txt, ~fontSize) => {
  let {sizeScaled: {heightScaled}} = Load.getCanvasData()
  let yCentered = 0.5 *. heightScaled
  txt->centerXText(~fontSize, ~y=yCentered)
}

// Displays the text for score and coins.
let scoreAndCoins = (score, coins) => {
  let fontSize = 10.
  let coin_string = coins->string_of_int
  let context = Load.getContext()
  let fontTxt = fontSize->int_of_float->string_of_int ++ "px"
  context.font = fontTxt ++ " 'Press Start 2P'"
  context.fillText(. "Cx" ++ coin_string, fontSize, fontSize *. 2.)
  let {sizeScaled: {widthScaled}} = Load.getCanvasData()
  let scoreTxt = string_of_int(score)
  context.fillText(.
    scoreTxt,
    widthScaled -. float_of_int(String.length(scoreTxt) + 1) *. fontSize,
    fontSize *. 2.,
  )
}

// Displays the fps.
let fps = fps_val => {
  let fontSize = 10.
  let fps_str = int_of_float(fps_val) |> string_of_int
  fps_str->centerXText(~fontSize, ~y=fontSize *. 2.)
}

let loggingIn = (~loadOrSave: Keys.loadOrSave) => {
  let fontSize = 10.
  ("Logging in before " ++
  switch loadOrSave {
  | Load => "loading"
  | Save => "saving"
  })->centerXYText(~fontSize)
}

let loading = () => {
  "Loading..."->centerXYText(~fontSize=10.)
}

let saving = () => {
  "Saving..."->centerXYText(~fontSize=10.)
}

let paused = () => {
  "Paused"->centerXYText(~fontSize=10.)
}

let blackScreen = texts => {
  let ctx = Load.getContext()
  let {sizeScaled: {widthScaled, heightScaled}} = Load.getCanvasData()
  ctx.rect(. 0., 0., widthScaled, heightScaled)
  ctx.fillStyle = "black"
  ctx.fill(.)
  ctx.fillStyle = "white"
  texts->List.forEach(((s, yPct)) => {
    s->centerXText(~fontSize=10., ~y=yPct *. heightScaled)
  })
  ctx.fillStyle = "black"
}

let levelFinished = (result: Actors.levelResult, level, elapsed) =>
  switch result {
  | Won => blackScreen(list{("You win level" ++ (level ++ "!"), 0.4), (elapsed, 0.6)})
  | Lost => blackScreen(list{("You lose level " ++ (level ++ "!"), 0.4), (elapsed, 0.6)})
  }

let particles = (particles: list<Particle.t>, ~viewport: Viewport.t) =>
  particles->List.forEach(part => {
    let x = part.px -. viewport.px
    and y = part.py -. viewport.py
    render(part.params.sprite, x, y)
  })

let object = (obj: Object.t, ~viewport: Viewport.t) => {
  let {x, y} = Viewport.fromCoord(viewport, obj.px, obj.py)
  obj.sprite->render(x, y)
  if Keys.checkBboxEnabled() {
    obj.sprite->renderBbox(x, y)
  }
}

let drawState = (state: State.t, ~fps as fps_) => {
  let objectsWithPlayers = {
    let objectsWihtPlayer1 = list{state.player1, ...state.objects}
    Keys.checkTwoPlayers() ? list{state.player2, ...objectsWihtPlayer1} : objectsWihtPlayer1
  }

  clearCanvas()
  drawBgd(state)
  objectsWithPlayers->List.forEach(obj => obj->object(~viewport=state.viewport))
  state.particles->particles(~viewport=state.viewport)
  fps(fps_)
  scoreAndCoins(state.score, state.coins)
}
