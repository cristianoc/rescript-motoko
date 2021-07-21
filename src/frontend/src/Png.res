let create = src => {
  let img = Html.createImg(Html.document)
  img.src = src
  img
}

let bgd1 = create("./sprites/" ++ "bgd-1.png")
let blocks = create("./sprites/" ++ "blocks.png")
let chunks = create("./sprites/" ++ "chunks.png")
let enemies = create("./sprites/" ++ "enemies.png")
let ground = create("./sprites/" ++ "ground.png")
let items = create("./sprites/" ++ "items.png")
let marioSmall = create("./sprites/" ++ "mario-small.png")
let mario2Small = create("./sprites/" ++ "mario2-small.png")
let marioBig = create("./sprites/" ++ "mario-big.png")
let mario2Big = create("./sprites/" ++ "mario2-big.png")
let panel = create("./sprites/" ++ "panel.png")
let score = create("./sprites/" ++ "score.png")

let toImg = t =>
  switch t {
  | Types.Bgd1 => bgd1
  | Blocks => blocks
  | Chunks => chunks
  | Enemies => enemies
  | Ground => ground
  | Items => items
  | MarioSmall => marioSmall
  | Mario2Small => mario2Small
  | MarioBig => marioBig
  | Mario2Big => mario2Big
  | Panel => panel
  | Score => score
  }
