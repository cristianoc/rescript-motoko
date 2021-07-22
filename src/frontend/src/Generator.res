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

let convertCoinToObj = ((_, x, y), ~state) => {
  let obj = Object.make(
    ~hasGravity=false,
    ~objTyp=Item(Coin),
    ~spriteParams=Sprite.makeParams(Coin),
    ~state,
    x,
    y,
  )
  obj
}

let addCoins = (state: Types.state, x, y0) => {
  let y = y0 -. 16.
  if Random.bool() && (trimEdge(x, y, ~level=state.level) && !(state.objects->memPos(x, y))) {
    state.objects->Js.Array2.push((Types.QBlockCoin, x, y)->convertCoinToObj(~state))->ignore
  }
}

let convertEnemyToObj = ((enemyTyp, x, y), ~state) => {
  let obj = Object.make(
    ~objTyp=Enemy(enemyTyp),
    ~spriteParams=Sprite.enemyParams(enemyTyp, Left),
    ~state,
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

let addEnemyOnBlock = (state: Types.state, x, y) => {
  let placeEnemy = Random.int(Config.enemyDensity(~level=state.level))
  if placeEnemy == 0 && !(state.objects->memPos(x, y -. 16.)) {
    state.objects
    ->Js.Array2.push((randomEnemyTyp(), x, y -. 16.)->convertEnemyToObj(~state))
    ->ignore
  }
}

let addBlock = (state: Types.state, blockTyp, xBlock, yBlock) => {
  let x = xBlock *. 16.
  let y = yBlock *. 16.
  if !(state.objects->memPos(x, y)) && trimEdge(x, y, ~level=state.level) {
    let obj = Object.make(
      ~objTyp=Block(blockTyp),
      ~spriteParams=Sprite.blockParams(blockTyp),
      ~state,
      x,
      y,
    )
    state.objects->Js.Array2.push(obj)->ignore
    state->addCoins(x, y)
    state->addEnemyOnBlock(x, y)
  }
}

// Generate a stair formation with block typ being dependent on typ. This type
// of stair formation requires that the first step be on the ground.
let generateGroundStairs = (cbx, cby, typ, ~state) => {
  state->addBlock(typ, cbx, cby)
  state->addBlock(typ, cbx +. 1., cby)
  state->addBlock(typ, cbx +. 2., cby)
  state->addBlock(typ, cbx +. 3., cby)
  state->addBlock(typ, cbx +. 1., cby -. 1.)
  state->addBlock(typ, cbx +. 2., cby -. 1.)
  state->addBlock(typ, cbx +. 3., cby -. 1.)
  state->addBlock(typ, cbx +. 2., cby -. 2.)
  state->addBlock(typ, cbx +. 3., cby -. 2.)
  state->addBlock(typ, cbx +. 3., cby -. 3.)
}

// Generate a stair formation going upwards.
let generateAirupStairs = (cbx, cby, typ, ~state) => {
  state->addBlock(typ, cbx, cby)
  state->addBlock(typ, cbx +. 1., cby)
  state->addBlock(typ, cbx +. 3., cby -. 1.)
  state->addBlock(typ, cbx +. 4., cby -. 1.)
  state->addBlock(typ, cbx +. 4., cby -. 2.)
  state->addBlock(typ, cbx +. 5., cby -. 2.)
  state->addBlock(typ, cbx +. 6., cby -. 2.)
}

// Generate a stair formation going downwards
let generateAirdownStairs = (cbx, cby, typ, ~state) => {
  state->addBlock(typ, cbx, cby)
  state->addBlock(typ, cbx +. 1., cby)
  state->addBlock(typ, cbx +. 2., cby)
  state->addBlock(typ, cbx +. 2., cby +. 1.)
  state->addBlock(typ, cbx +. 3., cby +. 1.)
  state->addBlock(typ, cbx +. 5., cby +. 2.)
  state->addBlock(typ, cbx +. 6., cby +. 2.)
}

// Generate a cloud block platform with some length num.
let rec generateClouds = (cbx, cby, typ, num, ~state) =>
  if num == 0 {
    ()
  } else {
    state->addBlock(typ, cbx, cby)
    generateClouds(cbx +. 1., cby, typ, num - 1, ~state)
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
let chooseBlockPattern = (cbx: float, cby: float, ~state: Types.state) =>
  if cbx > Config.blockw(~level=state.level) || cby > Config.blockh(~level=state.level) {
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
      state->addBlock(stairTyp, cbx, cby)
      state->addBlock(middleBlock, cbx +. 1., cby)
      state->addBlock(stairTyp, cbx +. 2., cby)
    | 1 =>
      let numClouds = Random.int(5) + 5
      if cby < 5. {
        generateClouds(cbx, cby, Cloud, numClouds, ~state)
      } else {
        ()
      }
    | 2 =>
      if Config.blockh(~level=state.level) -. cby == 1. {
        generateGroundStairs(cbx, cby, stairTyp, ~state)
      } else {
        ()
      }
    | 3 =>
      if stairTyp == Brick && Config.blockh(~level=state.level) -. cby > 3. {
        generateAirdownStairs(cbx, cby, stairTyp, ~state)
      } else if Config.blockh(~level=state.level) -. cby > 2. {
        generateAirupStairs(cbx, cby, stairTyp, ~state)
      } else {
        state->addBlock(stairTyp, cbx, cby)
      }
    | _ =>
      if cby +. 3. -. Config.blockh(~level=state.level) == 2. {
        state->addBlock(stairTyp, cbx, cby)
      } else if cby +. 3. -. Config.blockh(~level=state.level) == 1. {
        state->addBlock(stairTyp, cbx, cby)
        state->addBlock(stairTyp, cbx, cby +. 1.)
      } else {
        state->addBlock(stairTyp, cbx, cby)
        state->addBlock(stairTyp, cbx, cby +. 1.)
        state->addBlock(stairTyp, cbx, cby +. 2.)
      }
    }
  }

// Generates a list of enemies to be placed on the ground.
let rec generateEnemiesOnGround = (cbx: float, cby: float, ~state: Types.state) =>
  if cbx > Config.blockw(~level=state.level) -. 32. {
    ()
  } else if cby > Config.blockh(~level=state.level) -. 1. || cbx < 15. {
    generateEnemiesOnGround(cbx +. 1., 0., ~state)
  } else if cby == 0. || (Config.blockh(~level=state.level) -. 1. != cby || Random.int(10) != 0) {
    generateEnemiesOnGround(cbx, cby +. 1., ~state)
  } else {
    state.objects
    ->Js.Array2.push((randomEnemyTyp(), cbx *. 16., cby *. 16.)->convertEnemyToObj(~state))
    ->ignore
    generateEnemiesOnGround(cbx, cby +. 1., ~state)
  }

// Generate an objCoord list (typ, coordinates) of blocks to be placed.
let rec generateBlocks = (cbx: float, cby: float, ~state: Types.state) =>
  if Config.blockw(~level=state.level) -. cbx < 33. {
    ()
  } else if cby > Config.blockh(~level=state.level) -. 1. {
    generateBlocks(cbx +. 1., 0., ~state)
  } else if state.objects->memPos(cbx, cby) || cby == 0. {
    generateBlocks(cbx, cby +. 1., ~state)
  } else if Random.int(20) == 0 {
    chooseBlockPattern(cbx, cby, ~state)
    generateBlocks(cbx, cby +. 1., ~state)
  } else {
    generateBlocks(cbx, cby +. 1., ~state)
  }

// Generate the ending item panel at the end of the level. Games ends upon
// collision with player.
let generatePanel = (~state): Types.obj => {
  let obj = Object.make(
    ~objTyp=Block(Panel),
    ~spriteParams=Sprite.blockParams(Panel),
    ~state,
    Config.blockw(~level=state.level) *. 16. -. 256.,
    Config.blockh(~level=state.level) *. 16. *. 2. /. 3.,
  )
  obj
}

let convertBlockToObj = ((blockTyp, x, y)) => {
  let obj = Object.make(~objTyp=Block(blockTyp), ~spriteParams=Sprite.blockParams(blockTyp), x, y)
  obj
}

// Generate the list of brick locations needed to display the ground.
// 1/10 chance that a ground block is skipped each call to create holes.
let rec generateGround = (inc: float, ~state: Types.state) =>
  if inc > Config.blockw(~level=state.level) {
    ()
  } else if inc > 10. {
    let skip = Random.int(10)
    if skip == 7 && Config.blockw(~level=state.level) -. inc > 32. {
      generateGround(inc +. 1., ~state)
    } else {
      state.objects
      ->Js.Array2.push(
        (Ground, inc *. 16., Config.blockh(~level=state.level) *. 16.)->convertBlockToObj(~state),
      )
      ->ignore
      generateGround(inc +. 1., ~state)
    }
  } else {
    state.objects
    ->Js.Array2.push(
      (Ground, inc *. 16., Config.blockh(~level=state.level) *. 16.)->convertBlockToObj(~state),
    )
    ->ignore
    generateGround(inc +. 1., ~state)
  }

// Procedurally generate a list of objects given canvas width, height and
// context. Arguments block width (blockw) and block height (blockh) are in
// block form, not pixels.
let generateHelper = (~state) => {
  generateBlocks(0., 0., ~state)
  generateGround(0., ~state)
  generateEnemiesOnGround(0., 0., ~state)
  let panel = generatePanel(~state)
  state.objects->Js.Array2.push(panel)->ignore
}

let newPlayer = (playerNum: Types.playerNum, ~state) =>
  Object.make(
    ~state,
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
let generate = (~state: Types.state) => {
  Random.init(Config.randomSeed(~level=state.level))
  let initial = Html.performance.now(.)
  generateHelper(~state)
  let elapsed = Html.performance.now(.) -. initial
  Js.log3(
    "generated",
    state.objects |> Js.Array2.length,
    "objects in " ++ (Js.Float.toString(elapsed) ++ " milliseconds"),
  )
}
