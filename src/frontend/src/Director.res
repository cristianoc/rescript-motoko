open Belt

let collidObjs = ref(list{}) // List of next iteration collidable objects

let lastTime = ref(0.) // Used for calculating fps
let initialTime = ref(0.) // Used for calculating fps

// Calculate fps as the difference between [t0] and [t1]
let calcFps = () => {
  let t0 = lastTime.contents
  let time = Html.performance.now(.)
  lastTime := time
  if t0 == 0. {
    initialTime := time
    0.
  } else {
    let delta = (time -. t0) /. 1000.
    time -. initialTime.contents < 1000.0 ? 0. : 1. /. delta
  }
}

// playerAttackEnemy is called for a player hitting an enemy from the north.
// This causes the player to either kill the enemy or move the enemy, in the
// case that the enemy is a shell. Invulnerability, jumping, and grounded
// are used for fine tuning the movements.
let playerAttackEnemy = (o1, enemyTyp: Actors.enemyTyp, s2, o2, state: State.t) => {
  o1.Object.invuln = 10
  o1.jumping = false
  o1.grounded = true
  switch enemyTyp {
  | GKoopaShell | RKoopaShell =>
    let r2 = Object.evolveEnemy(o1.dir, enemyTyp, s2, o2)
    o1.vy = -.Config.dampenJump
    o1.py = o1.py -. 5.
    (None, r2)
  | _ =>
    Object.decHealth(o2)
    o1.vy = -.Config.dampenJump
    if state.multiplier == 8 {
      state->State.updateScore(800)
      o2.score = 800
      (None, Object.evolveEnemy(o1.dir, enemyTyp, s2, o2))
    } else {
      let score = 100 * state.multiplier
      state->State.updateScore(score)
      o2.score = score
      state.multiplier = state.multiplier * 2
      (None, Object.evolveEnemy(o1.dir, enemyTyp, s2, o2))
    }
  }
}

// enemyAttackPlayer is used when an enemy kills a player.
let enemyAttackPlayer = (o1: Object.t, enemy: Actors.enemyTyp, s2, o2: Object.t) =>
  switch enemy {
  | GKoopaShell | RKoopaShell =>
    let r2 = if o2.vx == 0. {
      Object.evolveEnemy(o1.dir, enemy, s2, o2)
    } else {
      Object.decHealth(o1)
      o1.invuln = Config.invuln
      None
    }
    (None, r2)
  | _ =>
    Object.decHealth(o1)
    o1.invuln = Config.invuln
    (None, None)
  }

// In the case that two enemies collide, they are to reverse directions. However,
// in the case that one or more of the two enemies is a koopa shell, then
// the koopa shell kills the other enemy.
let collEnemyEnemy = (
  enemy1: Actors.enemyTyp,
  s1,
  o1,
  enemy2: Actors.enemyTyp,
  s2,
  o2,
  dir: Actors.dir2d,
) =>
  switch (enemy1, enemy2) {
  | (GKoopaShell, GKoopaShell)
  | (GKoopaShell, RKoopaShell)
  | (RKoopaShell, RKoopaShell)
  | (RKoopaShell, GKoopaShell) =>
    Object.decHealth(o1)
    Object.decHealth(o2)
    (None, None)
  | (RKoopaShell, _) | (GKoopaShell, _) =>
    if o1.vx == 0. {
      Object.revDir(o2, enemy2, s2)
      (None, None)
    } else {
      Object.decHealth(o2)
      (None, None)
    }
  | (_, RKoopaShell) | (_, GKoopaShell) =>
    if o2.vx == 0. {
      Object.revDir(o1, enemy1, s1)
      (None, None)
    } else {
      Object.decHealth(o1)
      (None, None)
    }
  | (_, _) =>
    switch dir {
    | West | East =>
      Object.revDir(o1, enemy1, s1)
      Object.revDir(o2, enemy2, s2)
      (None, None)
    | _ => (None, None)
    }
  }

// Process collision is called to match each of the possible collisions that
// may occur. Returns a pair of options, representing objects that
// were created from the existing ones. That is, the first element represents
// a new item spawned as a result of the first object. None indicates that
// no new item should be spawned. Transformations to existing objects occur
// mutably, as many changes are side-effectual.
let processCollision = (dir: Actors.dir2d, obj1: Object.t, obj2: Object.t, state: State.t) =>
  switch (obj1, obj2, dir) {
  | ({objTyp: Player(_)}, {objTyp: Player(_)}, East | West) =>
    obj2.vx = obj2.vx +. obj1.vx
    (None, None)
  | ({objTyp: Player(_)}, {objTyp: Enemy(typ), sprite: s2}, South)
  | ({objTyp: Enemy(typ), sprite: s2}, {objTyp: Player(_)}, North) =>
    playerAttackEnemy(obj1, typ, s2, obj2, state)
  | ({objTyp: Player(_)}, {objTyp: Enemy(t2), sprite: s2}, _)
  | ({objTyp: Enemy(t2), sprite: s2}, {objTyp: Player(_)}, _) =>
    enemyAttackPlayer(obj1, t2, s2, obj2)
  | ({objTyp: Player(_)}, {objTyp: Item(t2)}, _) | ({objTyp: Item(t2)}, {objTyp: Player(_)}, _) =>
    switch t2 {
    | Mushroom =>
      Object.decHealth(obj2)
      if obj1.health == 2 {
        ()
      } else {
        obj1.health = obj1.health + 1
      }
      obj1.vx = 0.
      obj1.vy = 0.
      state->State.updateScore(1000)
      obj2.score = 1000
      (None, None)
    | Coin =>
      state.coins = state.coins + 1
      Object.decHealth(obj2)
      state->State.updateScore(100)
      (None, None)
    }
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Enemy(t2), sprite: s2}, dir) =>
    collEnemyEnemy(t1, s1, obj1, t2, s2, obj2, dir)
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Block(t2)}, East)
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Block(t2)}, West) =>
    switch (t1, t2) {
    | (RKoopaShell, Brick) | (GKoopaShell, Brick) =>
      Object.decHealth(obj2)
      Object.reverseLeftRight(obj1)
      (None, None)
    | (RKoopaShell, QBlock(typ)) | (GKoopaShell, QBlock(typ)) =>
      let updatedBlock = Object.evolveBlock(obj2)
      let spawnedItem = Object.spawnAbove(obj1.dir, obj2, typ)
      Object.revDir(obj1, t1, s1)
      (Some(updatedBlock), Some(spawnedItem))
    | (_, _) =>
      Object.revDir(obj1, t1, s1)
      (None, None)
    }
  | ({objTyp: Item(_)}, {objTyp: Block(_)}, East) | ({objTyp: Item(_)}, {objTyp: Block(_)}, West) =>
    Object.reverseLeftRight(obj1)
    (None, None)
  | ({objTyp: Enemy(_)}, {objTyp: Block(_)}, _) | ({objTyp: Item(_)}, {objTyp: Block(_)}, _) =>
    Object.collideBlock(dir, obj1)
    (None, None)
  | ({objTyp: Player(t1, _)}, {objTyp: Block(t)}, North) =>
    switch t {
    | QBlock(typ) =>
      let updatedBlock = Object.evolveBlock(obj2)
      let spawnedItem = Object.spawnAbove(obj1.dir, obj2, typ)
      Object.collideBlock(dir, obj1)
      (Some(spawnedItem), Some(updatedBlock))
    | Brick =>
      if t1 == BigM {
        Object.collideBlock(dir, obj1)
        Object.decHealth(obj2)
        (None, None)
      } else {
        Object.collideBlock(dir, obj1)
        (None, None)
      }
    | Panel =>
      state.status = Finished({levelResult: Won, finishTime: Html.performance.now(.)})
      (None, None)
    | _ =>
      Object.collideBlock(dir, obj1)
      (None, None)
    }
  | ({objTyp: Player(_)}, {objTyp: Block(t)}, _) =>
    switch t {
    | Panel =>
      state.status = Finished({levelResult: Won, finishTime: Html.performance.now(.)})
      (None, None)
    | _ =>
      switch dir {
      | South =>
        state.multiplier = 1
        Object.collideBlock(dir, obj1)
        (None, None)
      | _ =>
        Object.collideBlock(dir, obj1)
        (None, None)
      }
    }
  | (_, _, _) => (None, None)
  }

let viewportFilter = (obj: Object.t, viewport) =>
  Viewport.inViewport(viewport, obj.px, obj.py) ||
  (Object.isPlayer(obj) ||
  Viewport.outOfViewportBelow(viewport, obj.py))

// Run the broad phase object filtering
let broadPhase = (~allCollids, viewport) => allCollids->List.keep(o => o->viewportFilter(viewport))

// narrowPhase of collision is used in order to continuously loop through
// each of the collidable objects to constantly check if collisions are
// occurring.
let narrowPhase = (obj, cs, state) => {
  let rec narrowHelper = (obj: Object.t, cs, state, acc) =>
    switch cs {
    | list{} => acc
    | list{h, ...t} =>
      let newObjs = if !Object.equals(obj, h) {
        switch Object.checkCollision(obj, h) {
        | None => (None, None)
        | Some(dir) =>
          if h.id != obj.id {
            processCollision(dir, obj, h, state)
          } else {
            (None, None)
          }
        }
      } else {
        (None, None)
      }
      let acc = switch newObjs {
      | (None, Some(o)) => list{o, ...acc}
      | (Some(o), None) => list{o, ...acc}
      | (Some(o1), Some(o2)) => list{o1, o2, ...acc}
      | (None, None) => acc
      }
      narrowHelper(obj, t, state, acc)
    }
  narrowHelper(obj, cs, state, list{})
}

// This is an optimization setp to determine which objects require narrow phase
// checking. This excludes static objects, allowing collision to only be
// checked with moving objects. This method is called once per objects.
// Collision detection proceeds as follows:
// 1. Broad phase - filter objects that cannot possibly collide with
// this object.
// 2. Narrow phase - compare against all objects to determine whether there
// is a collision, and process the collision.
// This method returns a list of objects that are created, which should be
// added to the list of objects for the next iteration.
let checkCollisions = (obj, state: State.t, ~allCollids) =>
  switch obj.Object.objTyp {
  | Block(_) => list{}
  | _ =>
    let broad = broadPhase(~allCollids, state.viewport)
    narrowPhase(obj, broad, state)
  }

// primary update method for objects,
// checking the collision, updating the object, and drawing to the canvas
let updateObject0 = (~allCollids, obj: Object.t, ~state: State.t) => {
  /* TODO: optimize. Draw static elements only once */
  let spr = obj.sprite
  obj.invuln = if obj.invuln > 0 {
    obj.invuln - 1
  } else {
    0
  }
  if (!obj.kill || obj->Object.isPlayer) && obj->viewportFilter(state.viewport) {
    obj.grounded = false
    obj->Object.processObj(~level=state.level)
    // Run collision detection if moving object
    let evolved = obj->checkCollisions(state, ~allCollids)
    // Render and update animation
    let vptAdjXy = Viewport.fromCoord(state.viewport, obj.px, obj.py)
    Draw.render(spr, vptAdjXy.x, vptAdjXy.y)
    if Keys.checkBboxEnabled() {
      Draw.renderBbox(spr, vptAdjXy.x, vptAdjXy.y)
    }
    if obj.vx != 0. || !Object.isEnemy(obj) {
      Sprite.updateAnimation(spr)
    }
    evolved
  } else {
    list{}
  }
}

// used to update all of the objects at once. Primarily used
// as a wrapper method. This method is necessary to differentiate between
// the player collidable and the remaining collidables, as special operations
// such as viewport centering only occur with the player
let updateObject = (~allCollids, obj: Object.t, ~state) =>
  switch obj.objTyp {
  | Player(_, n) =>
    let keys = Keys.translateKeys(n)
    obj.crouch = false
    Object.updatePlayer(obj, n, keys)
    let evolved = obj->updateObject0(~allCollids, ~state)
    collidObjs := \"@"(evolved, collidObjs.contents)
  | _ =>
    let evolved = obj->updateObject0(~allCollids, ~state)
    if !obj.kill {
      collidObjs := list{obj, ...\"@"(evolved, collidObjs.contents)}
    }
    let newParts = if obj.kill {
      Object.kill(obj)
    } else {
      list{}
    }
    state.particles = \"@"(newParts, state.particles)
  }

// Primary update function to update and persist a particle
let updateParticle = (state: State.t, part) => {
  Particle.process(part)
  let x = part.px -. state.viewport.px
  and y = part.py -. state.viewport.py
  Draw.render(part.params.sprite, x, y)
  if !part.kill {
    state.particles = list{part, ...state.particles}
  }
}

let rec updateHelper = (~state: State.t) => {
  switch state.status {
  | _ if Keys.checkPaused() =>
    Draw.paused()
    state.objects = collidObjs.contents
    Html.requestAnimationFrame(_ => updateHelper(~state))

  | Finished({levelResult, finishTime})
    if Html.performance.now(.) -. finishTime > Config.delayWhenFinished =>
    let timeToStart = Config.restartAfter -. (Html.performance.now(.) -. finishTime) /. 1000.
    if timeToStart > 0. {
      Draw.levelFinished(
        levelResult,
        state.level->string_of_int,
        timeToStart->int_of_float->string_of_int,
      )
      state.objects = collidObjs.contents
      Html.requestAnimationFrame(_ => updateHelper(~state))
    } else {
      let level = levelResult == Won ? state.level + 1 : state.level
      let state = State.new(~level)
      updateHelper(~state)
    }

  | Playing | Finished(_) =>
    let fps = calcFps()
    let oldObjects = state.objects
    collidObjs := list{}
    let oldParticles = state.particles
    state.particles = list{}
    Draw.clearCanvas()
    /* Parallax background */
    let vposXInt = int_of_float(state.viewport.px /. 5.)
    let bgdWidth = int_of_float(fst(state.bgd.params.frameSize))
    Draw.drawBgd(state.bgd, @doesNotRaise float_of_int(mod(vposXInt, bgdWidth)))
    state.player1->updateObject(~allCollids=list{state.player2, ...oldObjects}, ~state)
    state.player2->updateObject(~allCollids=list{state.player1, ...oldObjects}, ~state)
    if state.player1.kill == true {
      switch state.status {
      | Finished({levelResult: Lost}) => ()
      | _ => state.status = Finished({levelResult: Lost, finishTime: Html.performance.now(.)})
      }
    }
    Viewport.update(state.viewport, state.player1.px, state.player1.py)
    oldObjects->List.forEach(obj => obj->updateObject(~allCollids=oldObjects, ~state))
    oldParticles->List.forEach(part => updateParticle(state, part))
    Draw.fps(fps)
    Draw.scoreAndCoins(state.score, state.coins)
    state.objects = collidObjs.contents
    Html.requestAnimationFrame(_ => updateHelper(~state))
  }
}

// updateLoop is constantly being called to check for collisions and to
// update each of the objects in the game.
let updateLoop = (~level) => {
  let state = State.new(~level)
  updateHelper(~state)
}
