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
  context.drawImage(. sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)
}

// Draws two background images, which needs to be done because of the
// constantly changing viewport, which is always at most going to be
// between two background images.
let drawBgd = (bgd, off_x) => {
  render(bgd, -.off_x, 0.)
  render(bgd, fst(bgd.params.frameSize) -. off_x, 0.)
}

// Used for animation updating. Canvas is cleared each frame and redrawn.
let clearCanvas = () => {
  let canvas = Load.getCanvas()
  let context = Load.getContext()
  let cwidth = float_of_int(canvas.width)
  let cheight = float_of_int(canvas.height)
  context.clearRect(. 0., 0., cwidth, cheight)
}

let scoreString = score => {
  let b = "       "
  let blen = String.length(b)
  let s = string_of_int(score)
  let slen = s->String.length
  if slen <= blen {
    String.sub(b, 0, blen - slen) ++ s
  } else {
    s
  }
}

// Displays the text for score and coins.
let scoreAndCoins = (score, coins) => {
  let coin_string = coins->string_of_int
  let context = Load.getContext()
  context.font = "10px 'Press Start 2P'"
  context.fillText(. "Cx" ++ coin_string, 8., 18.)
  context.fillText(. scoreString(score), 264., 18.)
}

// Displays the fps.
let fps = fps_val => {
  let fps_str = int_of_float(fps_val) |> string_of_int
  Load.getContext().fillText(. fps_str, 169., 18.)
}

let blackScreen = texts => {
  let ctx = Load.getContext()
  let fontSize = 20. /. Config.scale
  let fontTxt = fontSize->int_of_float->string_of_int ++ "px"
  ctx.rect(. 0., 0., 512. /. Config.scale, 512. /. Config.scale)
  ctx.fillStyle = "black"
  ctx.fill(.)
  ctx.fillStyle = "white"
  ctx.font = fontTxt ++ "'Press Start 2P'"
  texts->List.forEach(((s, x, y)) => ctx.fillText(. s, x /. Config.scale, y /. Config.scale))
  ctx.fillStyle = "black"
}

let levelFinished = (result: Actors.levelResult, level, elapsed) =>
  switch result {
  | Won => blackScreen(list{("You win level" ++ (level ++ "!"), 80., 100.), (elapsed, 230., 160.)})
  | Lost =>
    blackScreen(list{("You lose level " ++ (level ++ "!"), 80., 100.), (elapsed, 230., 160.)})
  }
