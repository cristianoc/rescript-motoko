open Belt

type loadOrSave = LoadState | SaveState

/* Represents the values of relevant key bindings. */
type keys = {
  mutable bbox: bool,
  mutable down1: bool,
  mutable down2: bool,
  mutable left1: bool,
  mutable left2: bool,
  mutable paused: bool,
  mutable pendingStateOperations: option<loadOrSave>,
  mutable right1: bool,
  mutable right2: bool,
  mutable twoPlayers: bool,
  mutable up1: bool,
  mutable up2: bool,
}

// pressedKeys instantiates the keys
let pressedKeys = {
  bbox: false,
  down1: false,
  down2: false,
  left1: false,
  left2: false,
  paused: false,
  pendingStateOperations: None,
  right1: false,
  right2: false,
  twoPlayers: false,
  up1: false,
  up2: false,
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
  | 88 /* KeyX */ => pressedKeys.down2 = true
  | 83 /* KeyS */ => pressedKeys.pendingStateOperations = SaveState->Some
  | 76 /* KeyL */ => pressedKeys.pendingStateOperations = LoadState->Some
  | 66 /* KeyB */ => pressedKeys.bbox = !pressedKeys.bbox
  | 80 /* KeyP */ => pressedKeys.paused = !pressedKeys.paused
  | 50 /* Digit2 */ => pressedKeys.twoPlayers = !pressedKeys.twoPlayers
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
  | 88 => pressedKeys.down2 = false
  | _ => ()
  }
  true
}

// Returns whether the bounding box should be drawn
let checkBboxEnabled = () => pressedKeys.bbox

let checkPaused = () => pressedKeys.paused

let checkTwoPlayers = () => pressedKeys.twoPlayers

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
