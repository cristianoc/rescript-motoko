open Belt

open Actors

type aabb = {
  center: xy,
  half: xy,
}

let idCounter = ref(min_int)

type objTyp =
  | Player(plTyp, playerNum)
  | Enemy(enemyTyp)
  | Item(itemTyp)
  | Block(blockTyp)

type t = {
  mutable objTyp: objTyp,
  mutable sprite: Sprite.t,
  mutable hasGravity: bool,
  mutable speed: float,
  id: int,
  mutable px: float, // x position
  mutable py: float, // y position
  mutable vx: float, // x velocity
  mutable vy: float, // y velocity
  mutable jumping: bool,
  mutable grounded: bool,
  mutable dir: Actors.dir1d,
  mutable invuln: int,
  mutable kill: bool,
  mutable health: int,
  mutable crouch: bool,
  mutable score: int,
}

/* Sets an object's x velocity to the speed specified in its params based on
 * its direction */
let setVelToSpeed = obj => {
  let speed = obj.speed
  switch obj.dir {
  | Left => obj.vx = -.speed
  | Right => obj.vx = speed
  }
}

/* The following make functions all set the objects' has_gravity and speed,
 * returning an [obj_params] that can be directly plugged into the [obj]
 * during creation. */
let makePlayer = o => o.speed = Config.playerSpeed

let makeItem = (o, t) =>
  switch t {
  | Mushroom => ()
  | Coin => o.hasGravity = false
  }

let makeEnemy = (o, t) =>
  switch t {
  | Goomba => ()
  | GKoopa => ()
  | RKoopa => ()
  | GKoopaShell => o.speed = 3.
  | RKoopaShell => o.speed = 3.
  }

let makeBlock = (o, t) =>
  switch t {
  | QBlock(_) | QBlockUsed | Brick | UnBBlock | Cloud | Panel | Ground => o.hasGravity = false
  }

/* Used in object creation and to compare two objects. */
let newId = () => {
  idCounter := idCounter.contents + 1
  idCounter.contents
}

let make = (~hasGravity=true, ~speed=1.0, ~dir=Left, objTyp, spriteParams, px, py) => {
  let newObj = {
    objTyp: objTyp,
    sprite: spriteParams->Sprite.makeFromParams,
    hasGravity: hasGravity,
    speed: speed,
    px: px,
    py: py,
    vx: 0.0,
    vy: 0.0,
    id: newId(),
    jumping: false,
    grounded: false,
    dir: dir,
    invuln: 0,
    kill: false,
    health: 1,
    crouch: false,
    score: 0,
  }
  switch objTyp {
  | Player(_) => newObj->makePlayer
  | Item(item) => newObj->makeItem(item)
  | Enemy(t) => newObj->makeEnemy(t)
  | Block(t) => newObj->makeBlock(t)
  }
  newObj
}

let isPlayer = x =>
  switch x {
  | {objTyp: Player(_)} => true
  | _ => false
  }

let isEnemy = x =>
  switch x {
  | {objTyp: Enemy(_)} => true
  | _ => false
  }

let equals = (col1, col2) => col1.id == col2.id

let jump = player => {
  player.jumping = true
  player.grounded = false
  player.vy = max(
    player.vy -. (Config.playerJump +. abs_float(player.vx) *. 0.25),
    Config.playerMaxJump,
  )
}

// Matches the controls being used and updates each of the player's params
let updatePlayerKeys = (player: t, controls: controls): unit => {
  let lr_acc = player.vx *. 0.2
  switch controls {
  | CLeft =>
    if !player.crouch {
      if player.vx > -.player.speed {
        player.vx = player.vx -. (0.4 +. abs_float(lr_acc))
      }
      player.dir = Left
    }
  | CRight =>
    if !player.crouch {
      if player.vx < player.speed {
        player.vx = player.vx +. (0.4 +. abs_float(lr_acc))
      }
      player.dir = Right
    }
  | CUp =>
    if !player.jumping && player.grounded {
      player->jump
    }
  | CDown =>
    if !player.jumping && player.grounded {
      player.crouch = true
    }
  }
}

// Used for sprite changing. If sprites change to different dimensions as a result
// of some action, the new sprite must be normalized so that things aren't
// jumpy
let normalizePos = (o, p1: Sprite.params, p2: Sprite.params) => {
  let (box1, boy1) = p1.bboxOffset
  and (box2, boy2) = p2.bboxOffset
  let (bw1, bh1) = p1.bboxSize
  and (bw2, bh2) = p2.bboxSize
  o.px = o.px -. (bw2 +. box2) +. (bw1 +. box1)
  o.py = o.py -. (bh2 +. boy2) +. (bh1 +. boy1)
}

// Update player is constantly being called to check for if big or small
// Mario sprites should be used
let updatePlayer = (player, playerNum, keys) => {
  let prev_jumping = player.jumping
  let prev_dir = player.dir
  and prev_vx = abs_float(player.vx)
  List.forEach(keys, updatePlayerKeys(player))
  let v = player.vx *. Config.friction
  let vel_damped = if abs_float(v) < 0.1 {
    0.
  } else {
    v
  }
  player.vx = vel_damped
  let plSize = if player.health <= 1 {
    SmallM
  } else {
    BigM
  }

  let playerTyp = if !prev_jumping && player.jumping {
    Some(Jumping)
  } else if (
    prev_dir != player.dir || (prev_vx == 0. && abs_float(player.vx) > 0. && !player.jumping)
  ) {
    Some(Running)
  } else if prev_dir != player.dir && (player.jumping && prev_jumping) {
    Some(Jumping)
  } else if player.vy == 0. && player.crouch {
    Some(Crouching)
  } else if player.vy == 0. && player.vx == 0. {
    Some(Standing)
  } else {
    None
  }
  switch playerTyp {
  | Some(playerTyp) =>
    let newSprite =
      Sprite.makePlayer(plSize, playerTyp, player.dir, ~playerNum)->Sprite.makeFromParams
    let newTyp = plSize
    normalizePos(player, player.sprite.params, newSprite.params)
    player.objTyp = Player(newTyp, playerNum)
    player.sprite = newSprite
  | None => ()
  }
}

// The following two helper methods update velocity and position of the player
let updateVel = obj =>
  if obj.grounded {
    obj.vy = 0.
  } else if obj.hasGravity {
    obj.vy = min(obj.vy +. Config.gravity +. abs_float(obj.vy) *. 0.01, Config.maxYVel)
  }

let updatePos = obj => {
  obj.px = obj.vx +. obj.px
  if obj.hasGravity {
    obj.py = obj.vy +. obj.py
  }
}

// Calls two above helper functions to update velocity and position of player
let processObj = (obj, ~level) => {
  updateVel(obj)
  updatePos(obj)
  if obj.py > Config.levelHeight(~level) {
    obj.kill = true
  }
}

// Check upon collision of block and updates the values of the object
let collideBlock = (dir, obj) =>
  switch dir {
  | North => obj.vy = -0.001
  | South =>
    obj.vy = 0.
    obj.grounded = true
    obj.jumping = false
  | East | West => obj.vx = 0.
  }

// Simple helper method that reverses the direction in question
let oppositeDir = dir =>
  switch dir {
  | Left => Right
  | Right => Left
  }

// Used for enemy-enemy collisions
let reverseLeftRight = obj => {
  obj.vx = -.obj.vx
  obj.dir = oppositeDir(obj.dir)
}

// Actually creates a new enemy and deletes the previous. The positions must be
// normalized. This method is typically called when enemies are killed and a
// new sprite must be used (i.e., koopa to koopa shell).
let evolveEnemy = (player_dir, typ, spr: Sprite.t, obj) =>
  switch typ {
  | GKoopa =>
    let newObj = make(
      ~speed=3.,
      ~dir=obj.dir,
      Enemy(GKoopaShell),
      Sprite.makeEnemy(GKoopaShell, obj.dir),
      obj.px,
      obj.py,
    )
    normalizePos(newObj, spr.params, newObj.sprite.params)
    Some(newObj)
  | RKoopa =>
    let newObj = make(
      ~speed=3.,
      ~dir=obj.dir,
      Enemy(RKoopaShell),
      Sprite.makeEnemy(RKoopaShell, obj.dir),
      obj.px,
      obj.py,
    )
    Some(newObj)
  | GKoopaShell | RKoopaShell =>
    obj.dir = player_dir
    if obj.vx != 0. {
      obj.vx = 0.
    } else {
      setVelToSpeed(obj)
    }
    None
  | _ =>
    obj.kill = true
    None
  }

// Update the direction of the sprite
let revDir = (o, t, s: Sprite.t) => {
  reverseLeftRight(o)
  let old_params = s.params
  Sprite.transformEnemy(t, s, o.dir)
  normalizePos(o, old_params, s.params)
}

// Used for killing enemies, or to make big Mario into small Mario
let decHealth = obj => {
  let health = obj.health - 1
  if health == 0 {
    obj.kill = true
  } else if obj.invuln == 0 {
    obj.health = health
  }
  if obj->isPlayer {
    obj->jump
  }
}

// Used for deleting a block and replacing it with a used block
let evolveBlock = obj => {
  decHealth(obj)
  let newObj = make(
    ~hasGravity=false,
    ~dir=obj.dir,
    Block(QBlockUsed),
    Sprite.makeBlock(QBlockUsed),
    obj.px,
    obj.py,
  )
  newObj
}

// Used for spawning items above question mark blocks
let spawnAbove = (player_dir, obj, itemTyp) => {
  let item = make(
    ~hasGravity=itemTyp != Coin,
    ~dir=Left,
    Item(itemTyp),
    Sprite.makeItem(itemTyp),
    obj.px,
    obj.py,
  )
  item.py = item.py -. snd(item.sprite.params.frameSize)
  item.dir = oppositeDir(player_dir)
  setVelToSpeed(item)
  item
}

// Used to get the bounding box
let getAabb = obj => {
  let sprParams = obj.sprite.params
  let (offx, offy) = sprParams.bboxOffset
  let (box, boy) = (obj.px +. offx, obj.py +. offy)
  let (sx, sy) = sprParams.bboxSize
  {
    center: {
      x: box +. sx /. 2.,
      y: boy +. sy /. 2.,
    },
    half: {
      x: sx /. 2.,
      y: sy /. 2.,
    },
  }
}

let colBypass = (c1, c2) =>
  c1.kill ||
  (c2.kill ||
  switch (c1.objTyp, c2.objTyp) {
  | (Item(_), Enemy(_)) | (Enemy(_), Item(_)) | (Item(_), Item(_)) => true
  | (Player(_), Enemy(_)) =>
    if c1.invuln > 0 {
      true
    } else {
      false
    }
  | _ => false
  })

// Used for checking if collisions occur. Compares half-widths and half-heights
// and adjusts for when collisions do occur, by changing position so that
// a second collision does not occur again immediately. This causes snapping
let checkCollision = (o1, o2) => {
  let b1 = getAabb(o1)
  and b2 = getAabb(o2)
  if colBypass(o1, o2) {
    None
  } else {
    let vx = b1.center.x -. b2.center.x
    let vy = b1.center.y -. b2.center.y
    let hwidths = b1.half.x +. b2.half.x
    let hheights = b1.half.y +. b2.half.y
    if abs_float(vx) < hwidths && abs_float(vy) < hheights {
      let ox = hwidths -. abs_float(vx)
      let oy = hheights -. abs_float(vy)
      if ox +. 0.2 > oy {
        if (
          // avoid spurious horizontal collisions with floors when oy is tiny
          vy > 0.
        ) {
          o1.py = o1.py +. oy
          Some(North)
        } else {
          o1.py = o1.py -. oy
          Some(South)
        }
      } else if vx > 0. {
        o1.px = o1.px +. ox
        Some(West)
      } else {
        o1.px = o1.px -. ox
        Some(East)
      }
    } else {
      None
    }
  }
}

// "Kills" the matched object by setting certain parameters for each
let kill = obj =>
  switch obj.objTyp {
  | Enemy(t) =>
    let score = if obj.score > 0 {
      list{Particle.makeScore(obj.score, obj.px, obj.py)}
    } else {
      list{}
    }
    let remains = switch t {
    | Goomba => list{Particle.make(GoombaSquish, obj.px, obj.py)}
    | _ => list{}
    }
    \"@"(score, remains)
  | Block(t) =>
    switch t {
    | Brick =>
      let p1 = Particle.make(~vel=(-5., -5.), ~acc=(0., 0.2), BrickChunkL, obj.px, obj.py)
      let p2 = Particle.make(~vel=(-3., -4.), ~acc=(0., 0.2), BrickChunkL, obj.px, obj.py)
      let p3 = Particle.make(~vel=(3., -4.), ~acc=(0., 0.2), BrickChunkR, obj.px, obj.py)
      let p4 = Particle.make(~vel=(5., -5.), ~acc=(0., 0.2), BrickChunkR, obj.px, obj.py)
      list{p1, p2, p3, p4}
    | _ => list{}
    }
  | Item(t) =>
    switch t {
    | Mushroom => list{Particle.makeScore(obj.score, obj.px, obj.py)}
    | _ => list{}
    }
  | _ => list{}
  }
