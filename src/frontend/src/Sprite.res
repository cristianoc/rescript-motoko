open Actors

type xy = (float, float)

type params = {
  maxFrames: int,
  maxTicks: int,
  img: Html.imageElement,
  frameSize: xy,
  srcOffset: xy,
  bboxOffset: xy,
  bboxSize: xy,
}

type t = {
  mutable params: params,
  mutable frame: int,
  mutable ticks: int,
}

// spriteParams is used to initialize a sprite.
let spriteParams = (
  ~bbOff as bboxOffset=(0., 0.),
  ~bbSz as bboxSize=(0., 0.),
  ~frameSize=(16., 16.),
  ~maxTicks=0,
  ~maxFrames=1,
  ~srcOffset,
  img,
) => {
  let bboxSize = if bboxSize == (0., 0.) {
    frameSize
  } else {
    bboxSize
  }
  let maxFrames = maxFrames < 1 ? 1 : maxFrames
  {
    img: img,
    maxFrames: maxFrames,
    maxTicks: maxTicks,
    frameSize: frameSize,
    srcOffset: srcOffset,
    bboxOffset: bboxOffset,
    bboxSize: bboxSize,
  }
}

module Png = {
  let create = src => {
    let img = Html.createImg(Html.document)
    img.src = src
    img
  }
  let marioSmall = create("./sprites/" ++ "mario-small.png")
  let mario2Small = create("./sprites/" ++ "mario2-small.png")
  let marioBig = create("./sprites/" ++ "mario-big.png")
  let mario2Big = create("./sprites/" ++ "mario2-big.png")
  let enemies = create("./sprites/" ++ "enemies.png")
  let items = create("./sprites/" ++ "items.png")
  let blocks = create("./sprites/" ++ "blocks.png")
  let panel = create("./sprites/" ++ "panel.png")
  let ground = create("./sprites/" ++ "ground.png")
  let chunks = create("./sprites/" ++ "chunks.png")
  let score = create("./sprites/" ++ "score.png")
  let bgd1 = create("./sprites/" ++ "bgd-1.png")
}

// The following functions are used in order to define sprite animations
// from their sprite sheets. Also creates bounding boxes if necessary.
// Sets sprite for small mario.
let smallPlayerParams = (typ, dir, ~playerNum) => {
  let png = switch playerNum {
  | One => Png.marioSmall
  | Two => Png.mario2Small
  }
  switch dir {
  /* 16x16 grid with 0x0 offset */
  | Left =>
    switch typ {
    | Standing => spriteParams(png, ~bbOff=(3., 1.), ~bbSz=(11., 15.), ~srcOffset=(0., 0.))
    | Jumping =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 15.),
        ~maxFrames=2,
        ~maxTicks=10,
        ~srcOffset=(16., 16.),
      )
    | Running =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 15.),
        ~maxFrames=3,
        ~maxTicks=5,
        ~srcOffset=(16., 0.),
      )
    | Crouching => spriteParams(png, ~bbOff=(1., 5.), ~bbSz=(14., 10.), ~srcOffset=(0., 64.))
    }
  | Right =>
    switch typ {
    | Standing => spriteParams(png, ~bbOff=(1., 1.), ~bbSz=(11., 15.), ~srcOffset=(0., 32.))
    | Jumping =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 15.),
        ~maxFrames=2,
        ~maxTicks=10,
        ~srcOffset=(16., 48.),
      )
    | Running =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 15.),
        ~maxFrames=3,
        ~maxTicks=5,
        ~srcOffset=(16., 32.),
      )
    | Crouching => spriteParams(png, ~bbOff=(1., 5.), ~bbSz=(14., 10.), ~srcOffset=(0., 64.))
    }
  }
}

// Sets sprite for big mario
let bigPlayerParams = (typ, dir, ~playerNum) => {
  let png = switch playerNum {
  | One => Png.marioBig
  | Two => Png.mario2Big
  }
  switch dir {
  | Left =>
    switch typ {
    | Standing =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 27.),
        ~srcOffset=(16., 5.),
      )
    | Jumping =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 25.),
        ~frameSize=(16., 26.),
        ~srcOffset=(48., 6.),
      )
    | Running =>
      spriteParams(
        png,
        ~maxFrames=4,
        ~maxTicks=10,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 27.),
        ~srcOffset=(0., 37.),
      )
    | Crouching =>
      spriteParams(
        png,
        ~bbOff=(2., 10.),
        ~bbSz=(13., 17.),
        ~frameSize=(16., 27.),
        ~srcOffset=(32., 5.),
      )
    }
  | Right =>
    switch typ {
    | Standing =>
      spriteParams(
        png,
        ~bbOff=(1., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 26.),
        ~srcOffset=(16., 69.),
      )
    | Jumping =>
      spriteParams(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 25.),
        ~frameSize=(16., 26.),
        ~srcOffset=(48., 70.),
      )
    | Running =>
      spriteParams(
        png,
        ~maxFrames=4,
        ~maxTicks=10,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 27.),
        ~srcOffset=(0., 101.),
      )
    | Crouching =>
      spriteParams(
        png,
        ~bbOff=(2., 10.),
        ~bbSz=(13., 17.),
        ~frameSize=(16., 27.),
        ~srcOffset=(32., 69.),
      )
    }
  }
}

// Set sprites for enemies: Goomba, Red Koopa, Green Koopa.
let enemyParams = (typ, dir) =>
  switch (typ, dir) {
  | (Goomba, _) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(1., 1.),
      ~bbSz=(14., 14.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~srcOffset=(0., 128.),
    )
  | (GKoopa, Left) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(4., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(0., 69.),
    )
  | (GKoopa, Right) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(1., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(32., 69.),
    )
  | (RKoopa, Left) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(4., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(0., 5.),
    )
  | (RKoopa, Right) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(1., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(32., 5.),
    )
  | (GKoopaShell, _) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(2., 2.),
      ~bbSz=(12., 13.),
      ~maxFrames=4,
      ~maxTicks=10,
      ~srcOffset=(0., 96.),
    )
  | (RKoopaShell, _) =>
    spriteParams(
      Png.enemies,
      ~bbOff=(2., 2.),
      ~bbSz=(12., 13.),
      ~maxFrames=4,
      ~maxTicks=10,
      ~srcOffset=(0., 32.),
    )
  }

// Set sprites for items: coin, fireflower, mushroom, star
let makeParams = x =>
  /* 16x16 grid with 0x0 offset */
  switch x {
  | Coin =>
    spriteParams(
      Png.items,
      ~bbOff=(3., 0.),
      ~bbSz=(12., 16.),
      ~maxFrames=3,
      ~maxTicks=15,
      ~srcOffset=(0., 80.),
    )
  | Mushroom => spriteParams(Png.items, ~bbOff=(2., 0.), ~bbSz=(12., 16.), ~srcOffset=(0., 0.))
  }

let brickParams = spriteParams(Png.blocks, ~maxFrames=5, ~maxTicks=10, ~srcOffset=(0., 0.))

let qBlockParams = spriteParams(Png.blocks, ~maxFrames=4, ~maxTicks=15, ~srcOffset=(0., 16.))

let qBlockUsedParams = spriteParams(Png.blocks, ~srcOffset=(0., 32.))

let unBBlockParams = spriteParams(Png.blocks, ~srcOffset=(0., 48.))

let cloudParams = spriteParams(Png.blocks, ~srcOffset=(0., 64.))

let panelParams = spriteParams(
  Png.panel,
  ~maxFrames=3,
  ~maxTicks=15,
  ~frameSize=(26., 26.),
  ~srcOffset=(0., 0.),
)

let groundParams = spriteParams(Png.ground, ~srcOffset=(0., 32.))

// Set sprites for blocks: brick, question block, unbreakable block, cloud block
// panel block, ground block.*/
let blockParams = x =>
  /* 16x16 grid with 0x0 offset */
  switch x {
  | Brick => brickParams
  | QBlock(_) => qBlockParams
  | QBlockUsed => qBlockUsedParams
  | UnBBlock => unBBlockParams
  | Cloud => cloudParams
  | Panel => panelParams
  | Ground => groundParams
  }

// Set sprites for particles, squished goomba, brick chunks (upon destruction
// of brick), score text.
let particleParams = x =>
  switch x {
  | GoombaSquish => spriteParams(Png.enemies, ~srcOffset=(0., 144.))
  | BrickChunkL => spriteParams(Png.chunks, ~frameSize=(8., 8.), ~srcOffset=(0., 0.))
  | BrickChunkR => spriteParams(Png.chunks, ~frameSize=(8., 8.), ~srcOffset=(8., 0.))
  | Score100 => spriteParams(Png.score, ~frameSize=(12., 8.), ~srcOffset=(0., 0.))
  | Score200 => spriteParams(Png.score, ~frameSize=(12., 9.), ~srcOffset=(0., 9.))
  | Score400 => spriteParams(Png.score, ~frameSize=(12., 9.), ~srcOffset=(0., 18.))
  | Score800 => spriteParams(Png.score, ~frameSize=(12., 9.), ~srcOffset=(0., 27.))
  | Score1000 => spriteParams(Png.score, ~frameSize=(14., 9.), ~srcOffset=(13., 0.))
  | Score2000 => spriteParams(Png.score, ~frameSize=(14., 9.), ~srcOffset=(13., 9.))
  | Score4000 => spriteParams(Png.score, ~frameSize=(14., 9.), ~srcOffset=(13., 18.))
  | Score8000 => spriteParams(Png.score, ~frameSize=(14., 9.), ~srcOffset=(13., 27.))
  }

// Call to set sprite for either big or small mario.
let playerParams = (plSize, typ, dir, ~playerNum) =>
  switch plSize {
  | BigM => bigPlayerParams(typ, dir, ~playerNum)
  | SmallM => smallPlayerParams(typ, dir, ~playerNum)
  }

// Make a sprite from provided [params]
let makeFromParams = params => {
  {params: params, frame: 0, ticks: 0}
}

// Make a background
let makeBgd = () => {
  let params = spriteParams(Png.bgd1, ~frameSize=(512., 256.), ~srcOffset=(0., 0.))
  makeFromParams(params)
}

// Make a particle from the given particle type
let makeParticle = ptyp => {
  let params = particleParams(ptyp)
  makeFromParams(params)
}

// used in order to switch the direction an enemy faces
let transformEnemy = (enemy_typ, spr, dir) => {
  let params = enemyParams(enemy_typ, dir)
  spr.params = params
}

// main method to cycle through sprite animations
let updateAnimation = (spr: t) => {
  /* Only advance frame when ticked */
  let curr_ticks = spr.ticks
  if curr_ticks >= spr.params.maxTicks {
    spr.ticks = 0
    spr.frame = @doesNotRaise mod(spr.frame + 1, spr.params.maxFrames)
  } else {
    spr.ticks = curr_ticks + 1
  }
}
