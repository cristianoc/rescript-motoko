// Note: Canvas is 512 by 256 (w*h) -> 32 by 16 blocks
// Holds obj typ and its coordinates. (int, (x-coord, y-coord))
type blockCoord = (Types.blockTyp, float, float)
type enemyCoord = (Types.enemyTyp, float, float)

let memPos = (objs: array<Types.obj>, x, y): bool =>
  objs->Js.Array2.some(({px, py}) => x == px && y == py)

// Get rid of objects with coordinates in the ending frame, within 128 pixels of
// the start, at the very top, and two blocks from the ground.
let trimEdge = (x, y, ~level) => {
  let pixx = Config.blockw(~level) *. 16.
  let pixy = Config.blockh(~level) *. 16.
  !(x < 128. || (pixx -. x < 528. || (y == 0. || pixy -. y < 48.)))
}

let convertCoinToObj = ((_, x, y), ~level) => {
  let obj = Object.make(
    ~level,
    ~hasGravity=false,
    ~objTyp=Item(Coin),
    ~spriteParams=Sprite.makeParams(Coin),
    x,
    y,
  )
  obj
}

let addCoins = (objects, x, y0, ~idCounter, ~level) => {
  let y = y0 -. 16.
  if Random.bool() && (trimEdge(x, y, ~level) && !(objects->memPos(x, y))) {
    objects->Js.Array2.push((Types.QBlockCoin, x, y)->convertCoinToObj(~idCounter, ~level))->ignore
  }
}

let convertEnemyToObj = ((enemyTyp, x, y), ~idCounter, ~level) => {
  let obj = Object.make(
    ~idCounter,
    ~level,
    ~objTyp=Enemy(enemyTyp),
    ~spriteParams=Sprite.enemyParams(enemyTyp, Left),
    x,
    y,
  )
  obj->Object.setVelToSpeed
  obj
}

let randomEnemyTyp = () =>
  switch Random.int(3) {
  | 0 => Types.RKoopa
  | 1 => GKoopa
  | _ => Goomba
  }

let addEnemyOnBlock = (objects, x, y, ~idCounter, ~level) => {
  let placeEnemy = Random.int(Config.enemyDensity(~level))
  if placeEnemy == 0 && !(objects->memPos(x, y -. 16.)) {
    objects
    ->Js.Array2.push((randomEnemyTyp(), x, y -. 16.)->convertEnemyToObj(~idCounter, ~level))
    ->ignore
  }
}

let addBlock = (objects, blockTyp, xBlock, yBlock, ~idCounter, ~level) => {
  let x = xBlock *. 16.
  let y = yBlock *. 16.
  if !(objects->memPos(x, y)) && trimEdge(x, y, ~level) {
    let obj = Object.make(
      ~idCounter,
      ~level,
      ~objTyp=Block(blockTyp),
      ~spriteParams=Sprite.blockParams(blockTyp),
      x,
      y,
    )
    objects->Js.Array2.push(obj)->ignore
    objects->addCoins(x, y, ~idCounter, ~level)
    objects->addEnemyOnBlock(x, y, ~idCounter, ~level)
  }
}

// Generate a stair formation with block typ being dependent on typ. This type
// of stair formation requires that the first step be on the ground.
let generateGroundStairs = (cbx, cby, typ, blocks, ~idCounter, ~level) => {
  blocks->addBlock(typ, cbx, cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 1., cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 2., cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 3., cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 1., cby -. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 2., cby -. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 3., cby -. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 2., cby -. 2., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 3., cby -. 2., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 3., cby -. 3., ~idCounter, ~level)
}

// Generate a stair formation going upwards.
let generateAirupStairs = (cbx, cby, typ, blocks, ~idCounter, ~level) => {
  blocks->addBlock(typ, cbx, cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 1., cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 3., cby -. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 4., cby -. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 4., cby -. 2., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 5., cby -. 2., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 6., cby -. 2., ~idCounter, ~level)
}

// Generate a stair formation going downwards
let generateAirdownStairs = (cbx, cby, typ, blocks, ~idCounter, ~level) => {
  blocks->addBlock(typ, cbx, cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 1., cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 2., cby, ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 2., cby +. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 3., cby +. 1., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 5., cby +. 2., ~idCounter, ~level)
  blocks->addBlock(typ, cbx +. 6., cby +. 2., ~idCounter, ~level)
}

// Generate a cloud block platform with some length num.
let rec generateClouds = (cbx, cby, typ, num, blocks, ~idCounter, ~level) =>
  if num == 0 {
    ()
  } else {
    blocks->addBlock(typ, cbx, cby, ~idCounter, ~level)
    generateClouds(cbx +. 1., cby, typ, num - 1, blocks, ~idCounter, ~level)
  }

let randomStairTyp = () => Random.bool() ? Types.UnBBlock : Brick

// Choose the form of the blocks to be placed.
// When called, leaves a 1 block gap from canvas size.
// 1. If current xblock or yblock is greater than canvas width or height
// respectively, return an empty list.
// 2. If current xblock or yblock is within 10 blocks of the left and right sides
// of the level map, prevent any objects from being initialized.
// 3. Else call helper methods to created block formations and return objCoord
// slist.
let chooseBlockPattern = (cbx: float, cby: float, objects: array<Types.obj>, ~idCounter, ~level) =>
  if cbx > Config.blockw(~level) || cby > Config.blockh(~level) {
    ()
  } else {
    let stairTyp = randomStairTyp()
    let lifeBlock = Random.int(5) == 0
    let middleBlock = if lifeBlock {
      Types.QBlockMushroom
    } else {
      stairTyp
    }
    switch Random.int(5) {
    | 0 =>
      objects->addBlock(stairTyp, cbx, cby, ~idCounter, ~level)
      objects->addBlock(middleBlock, cbx +. 1., cby, ~idCounter, ~level)
      objects->addBlock(stairTyp, cbx +. 2., cby, ~idCounter, ~level)
    | 1 =>
      let numClouds = Random.int(5) + 5
      if cby < 5. {
        generateClouds(cbx, cby, Cloud, numClouds, objects, ~idCounter, ~level)
      } else {
        ()
      }
    | 2 =>
      if Config.blockh(~level) -. cby == 1. {
        generateGroundStairs(cbx, cby, stairTyp, objects, ~idCounter, ~level)
      } else {
        ()
      }
    | 3 =>
      if stairTyp == Brick && Config.blockh(~level) -. cby > 3. {
        generateAirdownStairs(cbx, cby, stairTyp, objects, ~idCounter, ~level)
      } else if Config.blockh(~level) -. cby > 2. {
        generateAirupStairs(cbx, cby, stairTyp, objects, ~idCounter, ~level)
      } else {
        objects->addBlock(stairTyp, cbx, cby, ~idCounter, ~level)
      }
    | _ =>
      if cby +. 3. -. Config.blockh(~level) == 2. {
        objects->addBlock(stairTyp, cbx, cby, ~idCounter, ~level)
      } else if cby +. 3. -. Config.blockh(~level) == 1. {
        objects->addBlock(stairTyp, cbx, cby, ~idCounter, ~level)
        objects->addBlock(stairTyp, cbx, cby +. 1., ~idCounter, ~level)
      } else {
        objects->addBlock(stairTyp, cbx, cby, ~idCounter, ~level)
        objects->addBlock(stairTyp, cbx, cby +. 1., ~idCounter, ~level)
        objects->addBlock(stairTyp, cbx, cby +. 2., ~idCounter, ~level)
      }
    }
  }

// Generates a list of enemies to be placed on the ground.
let rec generateEnemiesOnGround = (objects, cbx: float, cby: float, ~idCounter, ~level) =>
  if cbx > Config.blockw(~level) -. 32. {
    ()
  } else if cby > Config.blockh(~level) -. 1. || cbx < 15. {
    generateEnemiesOnGround(objects, cbx +. 1., 0., ~idCounter, ~level)
  } else if cby == 0. || (Config.blockh(~level) -. 1. != cby || Random.int(10) != 0) {
    generateEnemiesOnGround(objects, cbx, cby +. 1., ~idCounter, ~level)
  } else {
    objects
    ->Js.Array2.push(
      (randomEnemyTyp(), cbx *. 16., cby *. 16.)->convertEnemyToObj(~idCounter, ~level),
    )
    ->ignore
    generateEnemiesOnGround(objects, cbx, cby +. 1., ~idCounter, ~level)
  }

// Generate an objCoord list (typ, coordinates) of blocks to be placed.
let rec generateBlocks = (objects, cbx: float, cby: float, ~idCounter, ~level) =>
  if Config.blockw(~level) -. cbx < 33. {
    ()
  } else if cby > Config.blockh(~level) -. 1. {
    generateBlocks(objects, cbx +. 1., 0., ~idCounter, ~level)
  } else if objects->memPos(cbx, cby) || cby == 0. {
    generateBlocks(objects, cbx, cby +. 1., ~idCounter, ~level)
  } else if Random.int(20) == 0 {
    chooseBlockPattern(cbx, cby, objects, ~idCounter, ~level)
    generateBlocks(objects, cbx, cby +. 1., ~idCounter, ~level)
  } else {
    generateBlocks(objects, cbx, cby +. 1., ~idCounter, ~level)
  }

// Generate the ending item panel at the end of the level. Games ends upon
// collision with player.
let generatePanel = (~idCounter, ~level): Types.obj => {
  let obj = Object.make(
    ~idCounter,
    ~level,
    ~objTyp=Block(Panel),
    ~spriteParams=Sprite.blockParams(Panel),
    Config.blockw(~level) *. 16. -. 256.,
    Config.blockh(~level) *. 16. *. 2. /. 3.,
  )
  obj
}

let convertBlockToObj = ((blockTyp, x, y)) => {
  let obj = Object.make(~objTyp=Block(blockTyp), ~spriteParams=Sprite.blockParams(blockTyp), x, y)
  obj
}

// Generate the list of brick locations needed to display the ground.
// 1/10 chance that a ground block is skipped each call to create holes.
let rec generateGround = (objects, inc: float, ~idCounter, ~level) =>
  if inc > Config.blockw(~level) {
    ()
  } else if inc > 10. {
    let skip = Random.int(10)
    if skip == 7 && Config.blockw(~level) -. inc > 32. {
      generateGround(objects, inc +. 1., ~idCounter, ~level)
    } else {
      objects
      ->Js.Array2.push(
        (Ground, inc *. 16., Config.blockh(~level) *. 16.)->convertBlockToObj(~idCounter, ~level),
      )
      ->ignore
      generateGround(objects, inc +. 1., ~idCounter, ~level)
    }
  } else {
    objects
    ->Js.Array2.push(
      (Ground, inc *. 16., Config.blockh(~level) *. 16.)->convertBlockToObj(~idCounter, ~level),
    )
    ->ignore
    generateGround(objects, inc +. 1., ~idCounter, ~level)
  }

// Procedurally generate a list of objects given canvas width, height and
// context. Arguments block width (blockw) and block height (blockh) are in
// block form, not pixels.
let generateHelper = (~idCounter, ~level) => {
  let objects = []
  objects->generateBlocks(0., 0., ~idCounter, ~level)
  objects->generateGround(0., ~idCounter, ~level)
  objects->generateEnemiesOnGround(0., 0., ~idCounter, ~level)
  let panel = generatePanel(~idCounter, ~level)
  objects->Js.Array2.push(panel)->ignore
  objects
}

let newPlayer = (playerNum: Types.playerNum, ~idCounter, ~level) =>
  Object.make(
    ~idCounter,
    ~level,
    ~objTyp=switch playerNum {
    | One => Player1(SmallM)
    | Two => Player2(SmallM)
    },
    ~spriteParams=Sprite.playerParams(SmallM, Standing, Left, ~playerNum),
    100.,
    224.,
  )

// Main function called to procedurally generate the level map. w and h args
// are in pixel form. Converts to block form to call generateHelper. Spawns
// the list of objects received from generateHelper to display on canvas.
let generate = (~idCounter, ~level): array<Types.obj> => {
  Random.init(Config.randomSeed(~level))
  let initial = Html.performance.now(.)
  let objects = generateHelper(~idCounter, ~level)
  let elapsed = Html.performance.now(.) -. initial
  Js.log3(
    "generated",
    objects |> Js.Array2.length,
    "objects in " ++ (Js.Float.toString(elapsed) ++ " milliseconds"),
  )
  objects
}
