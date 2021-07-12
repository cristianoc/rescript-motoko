open Actors

type xy = (float, float)

type params = {
  maxFrames: int,
  maxTicks: int,
  imgSrc: string,
  frameSize: xy,
  srcOffset: xy,
  bboxOffset: xy,
  bboxSize: xy,
}

type t = {
  mutable params: params,
  mutable frame: int,
  mutable ticks: int,
  mutable img: Html.imageElement,
}

// setupSprite is used to initialize a sprite.
let setupSprite = (
  ~bbOff as bboxOffset=(0., 0.),
  ~bbSz as bboxSize=(0., 0.),
  ~frameSize=(16., 16.),
  ~maxTicks=0,
  ~maxFrames=1,
  ~srcOffset,
  imgSrc,
) => {
  let bboxSize = if bboxSize == (0., 0.) {
    frameSize
  } else {
    bboxSize
  }
  let maxFrames = maxFrames < 1 ? 1 : maxFrames
  let imgSrc = "./sprites/" ++ imgSrc
  {
    imgSrc: imgSrc,
    maxFrames: maxFrames,
    maxTicks: maxTicks,
    frameSize: frameSize,
    srcOffset: srcOffset,
    bboxOffset: bboxOffset,
    bboxSize: bboxSize,
  }
}

// The following functions are used in order to define sprite animations
// from their sprite sheets. Also creates bounding boxes if necessary.
// Sets sprite for small mario.
let makeSmallPlayer = (typ, dir, ~playerNum) => {
  let png = switch playerNum {
  | One => "mario-small.png"
  | Two => "mario2-small.png"
  }
  switch dir {
  /* 16x16 grid with 0x0 offset */
  | Left =>
    switch typ {
    | Standing => setupSprite(png, ~bbOff=(3., 1.), ~bbSz=(11., 15.), ~srcOffset=(0., 0.))
    | Jumping =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 15.),
        ~maxFrames=2,
        ~maxTicks=10,
        ~srcOffset=(16., 16.),
      )
    | Running =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 15.),
        ~maxFrames=3,
        ~maxTicks=5,
        ~srcOffset=(16., 0.),
      )
    | Crouching => setupSprite(png, ~bbOff=(1., 5.), ~bbSz=(14., 10.), ~srcOffset=(0., 64.))
    }
  | Right =>
    switch typ {
    | Standing => setupSprite(png, ~bbOff=(1., 1.), ~bbSz=(11., 15.), ~srcOffset=(0., 32.))
    | Jumping =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 15.),
        ~maxFrames=2,
        ~maxTicks=10,
        ~srcOffset=(16., 48.),
      )
    | Running =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 15.),
        ~maxFrames=3,
        ~maxTicks=5,
        ~srcOffset=(16., 32.),
      )
    | Crouching => setupSprite(png, ~bbOff=(1., 5.), ~bbSz=(14., 10.), ~srcOffset=(0., 64.))
    }
  }
}

// Sets sprite for big mario
let makeBigPlayer = (typ, dir, ~playerNum) => {
  let png = switch playerNum {
  | One => "mario-big.png"
  | Two => "mario2-big.png"
  }
  switch dir {
  | Left =>
    switch typ {
    | Standing =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 27.),
        ~srcOffset=(16., 5.),
      )
    | Jumping =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 25.),
        ~frameSize=(16., 26.),
        ~srcOffset=(48., 6.),
      )
    | Running =>
      setupSprite(
        png,
        ~maxFrames=4,
        ~maxTicks=10,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 27.),
        ~srcOffset=(0., 37.),
      )
    | Crouching =>
      setupSprite(
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
      setupSprite(
        png,
        ~bbOff=(1., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 26.),
        ~srcOffset=(16., 69.),
      )
    | Jumping =>
      setupSprite(
        png,
        ~bbOff=(2., 1.),
        ~bbSz=(12., 25.),
        ~frameSize=(16., 26.),
        ~srcOffset=(48., 70.),
      )
    | Running =>
      setupSprite(
        png,
        ~maxFrames=4,
        ~maxTicks=10,
        ~bbOff=(2., 1.),
        ~bbSz=(13., 25.),
        ~frameSize=(16., 27.),
        ~srcOffset=(0., 101.),
      )
    | Crouching =>
      setupSprite(
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
let makeEnemy = (typ, dir) =>
  switch (typ, dir) {
  | (Goomba, _) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(1., 1.),
      ~bbSz=(14., 14.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~srcOffset=(0., 128.),
    )
  | (GKoopa, Left) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(4., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(0., 69.),
    )
  | (GKoopa, Right) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(1., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(32., 69.),
    )
  | (RKoopa, Left) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(4., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(0., 5.),
    )
  | (RKoopa, Right) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(1., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(32., 5.),
    )
  | (GKoopaShell, _) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(2., 2.),
      ~bbSz=(12., 13.),
      ~maxFrames=4,
      ~maxTicks=10,
      ~srcOffset=(0., 96.),
    )
  | (RKoopaShell, _) =>
    setupSprite(
      "enemies.png",
      ~bbOff=(2., 2.),
      ~bbSz=(12., 13.),
      ~maxFrames=4,
      ~maxTicks=10,
      ~srcOffset=(0., 32.),
    )
  }

// Set sprites for items: coin, fireflower, mushroom, star
let makeItem = x =>
  /* 16x16 grid with 0x0 offset */
  switch x {
  | Coin =>
    setupSprite(
      "items.png",
      ~bbOff=(3., 0.),
      ~bbSz=(12., 16.),
      ~maxFrames=3,
      ~maxTicks=15,
      ~srcOffset=(0., 80.),
    )
  | Mushroom => setupSprite("items.png", ~bbOff=(2., 0.), ~bbSz=(12., 16.), ~srcOffset=(0., 0.))
  }

let brickParams = setupSprite("blocks.png", ~maxFrames=5, ~maxTicks=10, ~srcOffset=(0., 0.))

let qBlockParams = setupSprite("blocks.png", ~maxFrames=4, ~maxTicks=15, ~srcOffset=(0., 16.))

let qBlockUsedParams = setupSprite("blocks.png", ~srcOffset=(0., 32.))

let unBBlockParams = setupSprite("blocks.png", ~srcOffset=(0., 48.))

let cloudParams = setupSprite("blocks.png", ~srcOffset=(0., 64.))

let panelParams = setupSprite(
  "panel.png",
  ~maxFrames=3,
  ~maxTicks=15,
  ~frameSize=(26., 26.),
  ~srcOffset=(0., 0.),
)

let groundParams = setupSprite("ground.png", ~srcOffset=(0., 32.))

// Set sprites for blocks: brick, question block, unbreakable block, cloud block
// panel block, ground block.*/
let makeBlock = x =>
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
let makeParticle = x =>
  switch x {
  | GoombaSquish => setupSprite("enemies.png", ~srcOffset=(0., 144.))
  | BrickChunkL => setupSprite("chunks.png", ~frameSize=(8., 8.), ~srcOffset=(0., 0.))
  | BrickChunkR => setupSprite("chunks.png", ~frameSize=(8., 8.), ~srcOffset=(8., 0.))
  | Score100 => setupSprite("score.png", ~frameSize=(12., 8.), ~srcOffset=(0., 0.))
  | Score200 => setupSprite("score.png", ~frameSize=(12., 9.), ~srcOffset=(0., 9.))
  | Score400 => setupSprite("score.png", ~frameSize=(12., 9.), ~srcOffset=(0., 18.))
  | Score800 => setupSprite("score.png", ~frameSize=(12., 9.), ~srcOffset=(0., 27.))
  | Score1000 => setupSprite("score.png", ~frameSize=(14., 9.), ~srcOffset=(13., 0.))
  | Score2000 => setupSprite("score.png", ~frameSize=(14., 9.), ~srcOffset=(13., 9.))
  | Score4000 => setupSprite("score.png", ~frameSize=(14., 9.), ~srcOffset=(13., 18.))
  | Score8000 => setupSprite("score.png", ~frameSize=(14., 9.), ~srcOffset=(13., 27.))
  }

// Call to set sprite for either big or small mario.
let makePlayer = (plSize, typ, dir, ~playerNum) =>
  switch plSize {
  | BigM => makeBigPlayer(typ, dir, ~playerNum)
  | SmallM => makeSmallPlayer(typ, dir, ~playerNum)
  }

// Make a sprite from provided [params]
let makeFromParams = params => {
  let img = Html.createImg(Html.document)
  img.src = params.imgSrc
  {params: params, img: img, frame: 0, ticks: 0}
}

// Make a background
let makeBgd = () => {
  let params = setupSprite("bgd-1.png", ~frameSize=(512., 256.), ~srcOffset=(0., 0.))
  makeFromParams(params)
}

// Make a particle from the given particle type
let makeParticle = ptyp => {
  let params = makeParticle(ptyp)
  makeFromParams(params)
}

// used in order to switch the direction an enemy faces
let transformEnemy = (enemy_typ, spr, dir) => {
  let params = makeEnemy(enemy_typ, dir)
  let img = Html.createImg(Html.document)
  img.src = params.imgSrc
  spr.params = params
  spr.img = img
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
