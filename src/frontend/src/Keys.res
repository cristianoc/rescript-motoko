open Belt

/* Represents the values of relevant key bindings. */
type keys = {
  mutable left1: bool,
  mutable right1: bool,
  mutable up1: bool,
  mutable down1: bool,
  mutable left2: bool,
  mutable right2: bool,
  mutable up2: bool,
  mutable down2: bool,
  mutable bbox: bool,
  mutable paused: bool,
}

// pressedKeys instantiates the keys
let pressedKeys = {
  left1: false,
  right1: false,
  up1: false,
  down1: false,
  left2: false,
  right2: false,
  up2: false,
  down2: false,
  bbox: false,
  paused: false,
}

/* Keydown event handler translates a key press */
let keydown = evt => {
  let evt = Html.keyboardEventToJsObj(evt)
  let () = switch evt["keyCode"] {
  | 32 | 38 => pressedKeys.up1 = true
  | 87 /* KeyW */ => pressedKeys.up2 = true
  | 39 => pressedKeys.right1 = true
  | 68 /* KeyD */ => pressedKeys.right2 = true
  | 37 => pressedKeys.left1 = true
  | 65 /* KeyA */ => pressedKeys.left2 = true
  | 40 => pressedKeys.down1 = true
  | 83 /* KeyS */ => pressedKeys.down2 = true
  | 66 /* KeyB */ => pressedKeys.bbox = !pressedKeys.bbox
  | 80 /* KeyP */ => pressedKeys.paused = !pressedKeys.paused
  | _ => ()
  }
  true
}

/* Keyup event handler translates a key release */
let keyup = evt => {
  let evt = Html.keyboardEventToJsObj(evt)
  let () = switch evt["keyCode"] {
  | 32 | 38 => pressedKeys.up1 = false
  | 87 => pressedKeys.up2 = false
  | 39 => pressedKeys.right1 = false
  | 68 => pressedKeys.right2 = false
  | 37 => pressedKeys.left1 = false
  | 65 => pressedKeys.left2 = false
  | 40 => pressedKeys.down1 = false
  | 83 => pressedKeys.down2 = false
  | _ => ()
  }
  true
}

// Returns whether the bounding box should be drawn
let checkBboxEnabled = () => pressedKeys.bbox

let checkPaused = () => pressedKeys.paused

/* Converts a keypress to a list of control keys, allowing more than one key
 * to be processed each frame. */
let translateKeys = playerNum => {
  let k = pressedKeys
  let ctrls1 = list{(k.left1, Actors.CLeft), (k.right1, CRight), (k.up1, CUp), (k.down1, CDown)}
  let ctrls2 = list{(k.left2, Actors.CLeft), (k.right2, CRight), (k.up2, CUp), (k.down2, CDown)}
  List.reduce(playerNum == Actors.One ? ctrls1 : ctrls2, list{}, (a, x) =>
    if fst(x) {
      list{snd(x), ...a}
    } else {
      a
    }
  )
}
