open Belt

let renderBbox = (sprite: Types.sprite, posx, posy) => {
  let (bbox, bboy) = sprite.params.bboxOffset
  let (bbsx, bbsy) = sprite.params.bboxSize
  let context = Load.getContext()
  context.strokeStyle = "#FF0000"
  context.strokeRect(. posx +. bbox, posy +. bboy, bbsx, bbsy)
}

// Draws a sprite onto the canvas
let render = (sprite: Types.sprite, posx, posy) => {
  let (sx, sy) = sprite.params.srcOffset
  let (sw, sh) = sprite.params.frameSize
  let (dx, dy) = (posx, posy)
  let (dw, dh) = sprite.params.frameSize
  let sx = sx +. float_of_int(sprite.frame) *. sw
  let context = Load.getContext()
  context.drawImage(. sprite.params.png->Png.toImg, sx, sy, sw, sh, dx, dy, dw, dh)
}

// Draws two background images, which needs to be done because of the
// constantly changing viewport, which is always at most going to be
// between two background images.
let drawBgd = (bgd, off_x) => {
  render(bgd, -.off_x, 0.)
  render(bgd, fst(bgd.params.frameSize) -. off_x, 0.)
}

/* Parallax background */
let drawBgd = (state: Types.state) => {
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

let fontPx = Config.fontSize->int_of_float->string_of_int ++ "px"

let centerXText = (txt, ~y) => {
  let ctx = Load.getContext()
  let {sizeScaled: {widthScaled}} = Load.getCanvasData()
  ctx.font = fontPx ++ "'Press Start 2P'"
  let xCentered = (widthScaled -. Config.fontSize *. float_of_int(String.length(txt))) /. 2.
  ctx.fillText(. txt, xCentered, y)
}

let centerXYText = txt => {
  let {sizeScaled: {heightScaled}} = Load.getCanvasData()
  let yCentered = 0.5 *. heightScaled
  txt->centerXText(~y=yCentered)
}

// Displays the text for score and coins.
let scoreAndCoins = (score, coins) => {
  let coin_string = coins->string_of_int
  let context = Load.getContext()
  context.font = fontPx ++ " 'Press Start 2P'"
  context.fillText(. "Cx" ++ coin_string, Config.fontSize, Config.fontSize *. 2.)
  let {sizeScaled: {widthScaled}} = Load.getCanvasData()
  let scoreTxt = string_of_int(score)
  context.fillText(.
    scoreTxt,
    widthScaled -. float_of_int(String.length(scoreTxt) + 1) *. Config.fontSize,
    Config.fontSize *. 2.,
  )
}

// Displays the fps.
let fps = fps_val => {
  let fps_str = int_of_float(fps_val) |> string_of_int
  fps_str->centerXText(~y=Config.fontSize *. 2.)
}

let loggingIn = (~loadOrSave: Types.loadOrSave) => {
  ("Logging in before " ++
  switch loadOrSave {
  | Load => "loading"
  | Save => "saving"
  })->centerXYText
}

let loading = () => {
  "Loading..."->centerXYText
}

let saving = () => {
  "Saving..."->centerXYText
}

let paused = () => {
  "Paused"->centerXYText
}

let blackScreen = texts => {
  let ctx = Load.getContext()
  let {sizeScaled: {widthScaled, heightScaled}} = Load.getCanvasData()
  ctx.rect(. 0., 0., widthScaled, heightScaled)
  ctx.fillStyle = "black"
  ctx.fill(.)
  ctx.fillStyle = "white"
  texts->List.forEach(((s, yPct)) => {
    s->centerXText(~y=yPct *. heightScaled)
  })
  ctx.fillStyle = "black"
}

let levelFinished = (result: Types.levelResult, level, elapsed) =>
  switch result {
  | Won => blackScreen(list{("You win level" ++ (level ++ "!"), 0.4), (elapsed, 0.6)})
  | Lost => blackScreen(list{("You lose level " ++ (level ++ "!"), 0.4), (elapsed, 0.6)})
  }

let particles = (particles: list<Types.particle>, ~viewport: Types.viewport) =>
  particles->List.forEach(part => {
    let x = part.px -. viewport.px
    and y = part.py -. viewport.py
    render(part.sprite, x, y)
  })

let object = (obj: Types.obj, ~viewport: Types.viewport) => {
  let {x, y} = Viewport.fromCoord(viewport, obj.px, obj.py)
  obj.sprite->render(x, y)
  if Keys.checkBboxEnabled() {
    obj.sprite->renderBbox(x, y)
  }
}

let drawState = (state: Types.state, ~fps as fps_) => {
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
