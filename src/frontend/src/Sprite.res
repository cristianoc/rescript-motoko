// spriteParams is used to initialize a sprite.
let spriteParams = (
  ~bbOff as bboxOffset=(0., 0.),
  ~bbSz as bboxSize=(0., 0.),
  ~frameSize=(16., 16.),
  ~maxTicks=0,
  ~maxFrames=1,
  ~srcOffset,
  png,
) => {
  let bboxSize = if bboxSize == (0., 0.) {
    frameSize
  } else {
    bboxSize
  }
  let maxFrames = maxFrames < 1 ? 1 : maxFrames
  {
    Types.bboxOffset: bboxOffset,
    bboxSize: bboxSize,
    frameSize: frameSize,
    maxFrames: maxFrames,
    maxTicks: maxTicks,
    png: png,
    srcOffset: srcOffset,
  }
}

// The following functions are used in order to define sprite animations
// from their sprite sheets. Also creates bounding boxes if necessary.
// Sets sprite for small mario.
let smallPlayerParams = (typ, dir, ~playerNum) => {
  let png = switch playerNum {
  | Types.One => Types.MarioSmall
  | Two => Types.Mario2Small
  }
  switch dir {
  /* 16x16 grid with 0x0 offset */
  | Types.Left =>
    switch typ {
    | Types.Standing => spriteParams(png, ~bbOff=(3., 1.), ~bbSz=(11., 15.), ~srcOffset=(0., 0.))
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
let bigPlayerParams = (playerTyp: Types.playerTyp, dir: Types.dir, ~playerNum: Types.playerNum) => {
  let png: Types.png = switch playerNum {
  | One => MarioBig
  | Two => Mario2Big
  }
  switch dir {
  | Left =>
    switch playerTyp {
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
    switch playerTyp {
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
let enemyParams = (typ: Types.enemyTyp, dir: Types.dir) =>
  switch (typ, dir) {
  | (Goomba, _) =>
    spriteParams(
      Types.Enemies,
      ~bbOff=(1., 1.),
      ~bbSz=(14., 14.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~srcOffset=(0., 128.),
    )
  | (GKoopa, Left) =>
    spriteParams(
      Types.Enemies,
      ~bbOff=(4., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(0., 69.),
    )
  | (GKoopa, Right) =>
    spriteParams(
      Types.Enemies,
      ~bbOff=(1., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(32., 69.),
    )
  | (RKoopa, Left) =>
    spriteParams(
      Types.Enemies,
      ~bbOff=(4., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(0., 5.),
    )
  | (RKoopa, Right) =>
    spriteParams(
      Types.Enemies,
      ~bbOff=(1., 10.),
      ~bbSz=(11., 16.),
      ~maxFrames=2,
      ~maxTicks=10,
      ~frameSize=(16., 27.),
      ~srcOffset=(32., 5.),
    )
  | (GKoopaShell, _) =>
    spriteParams(
      Types.Enemies,
      ~bbOff=(2., 2.),
      ~bbSz=(12., 13.),
      ~maxFrames=4,
      ~maxTicks=10,
      ~srcOffset=(0., 96.),
    )
  | (RKoopaShell, _) =>
    spriteParams(
      Types.Enemies,
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
  | Types.Coin =>
    spriteParams(
      Types.Items,
      ~bbOff=(3., 0.),
      ~bbSz=(12., 16.),
      ~maxFrames=3,
      ~maxTicks=15,
      ~srcOffset=(0., 80.),
    )
  | Mushroom => spriteParams(Types.Items, ~bbOff=(2., 0.), ~bbSz=(12., 16.), ~srcOffset=(0., 0.))
  }

let brickParams = spriteParams(Types.Blocks, ~maxFrames=5, ~maxTicks=10, ~srcOffset=(0., 0.))

let qBlockParams = spriteParams(Types.Blocks, ~maxFrames=4, ~maxTicks=15, ~srcOffset=(0., 16.))

let qBlockUsedParams = spriteParams(Types.Blocks, ~srcOffset=(0., 32.))

let unBBlockParams = spriteParams(Types.Blocks, ~srcOffset=(0., 48.))

let cloudParams = spriteParams(Types.Blocks, ~srcOffset=(0., 64.))

let panelParams = spriteParams(
  Types.Panel,
  ~maxFrames=3,
  ~maxTicks=15,
  ~frameSize=(26., 26.),
  ~srcOffset=(0., 0.),
)

let groundParams = spriteParams(Types.Ground, ~srcOffset=(0., 32.))

// Set sprites for blocks: brick, question block, unbreakable block, cloud block
// panel block, ground block.*/
let blockParams = x =>
  /* 16x16 grid with 0x0 offset */
  switch x {
  | Types.Brick => brickParams
  | QBlockMushroom | QBlockCoin => qBlockParams
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
  | Types.GoombaSquish => spriteParams(Types.Enemies, ~srcOffset=(0., 144.))
  | BrickChunkL => spriteParams(Types.Chunks, ~frameSize=(8., 8.), ~srcOffset=(0., 0.))
  | BrickChunkR => spriteParams(Types.Chunks, ~frameSize=(8., 8.), ~srcOffset=(8., 0.))
  | Score100 => spriteParams(Types.Score, ~frameSize=(12., 8.), ~srcOffset=(0., 0.))
  | Score200 => spriteParams(Types.Score, ~frameSize=(12., 9.), ~srcOffset=(0., 9.))
  | Score400 => spriteParams(Types.Score, ~frameSize=(12., 9.), ~srcOffset=(0., 18.))
  | Score800 => spriteParams(Types.Score, ~frameSize=(12., 9.), ~srcOffset=(0., 27.))
  | Score1000 => spriteParams(Types.Score, ~frameSize=(14., 9.), ~srcOffset=(13., 0.))
  | Score2000 => spriteParams(Types.Score, ~frameSize=(14., 9.), ~srcOffset=(13., 9.))
  | Score4000 => spriteParams(Types.Score, ~frameSize=(14., 9.), ~srcOffset=(13., 18.))
  | Score8000 => spriteParams(Types.Score, ~frameSize=(14., 9.), ~srcOffset=(13., 27.))
  }

// Call to set sprite for either big or small mario.
let playerParams = (plSize, typ, dir, ~playerNum) =>
  switch plSize {
  | Types.BigM => bigPlayerParams(typ, dir, ~playerNum)
  | SmallM => smallPlayerParams(typ, dir, ~playerNum)
  }

// Make a sprite from provided [params]
let makeFromParams = params => {
  {Types.params: params, frame: 0, ticks: 0}
}

// Make a background
let makeBgd = () => {
  let params = spriteParams(Types.Bgd1, ~frameSize=(512., 256.), ~srcOffset=(0., 0.))
  makeFromParams(params)
}

// Make a particle from the given particle type
let makeParticle = ptyp => {
  let params = particleParams(ptyp)
  makeFromParams(params)
}

// used in order to switch the direction an enemy faces
let transformEnemy = (enemy_typ, sprite: Types.sprite, dir) => {
  let params = enemyParams(enemy_typ, dir)
  sprite.params = params
}

// main method to cycle through sprite animations
let updateAnimation = (sprite: Types.sprite) => {
  /* Only advance frame when ticked */
  let curr_ticks = sprite.ticks
  if curr_ticks >= sprite.params.maxTicks {
    sprite.ticks = 0
    sprite.frame = @doesNotRaise mod(sprite.frame + 1, sprite.params.maxFrames)
  } else {
    sprite.ticks = curr_ticks + 1
  }
}
