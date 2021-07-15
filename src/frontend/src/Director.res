open Belt

// Calculate fps as the difference between [t0] and [t1]
let calcFps = {
  let lastTime = ref(0.)
  let initialTime = ref(0.)
  () => {
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
}

// playerAttackEnemy is called for a player hitting an enemy from the north.
// This causes the player to either kill the enemy or move the enemy, in the
// case that the enemy is a shell. Invulnerability, jumping, and grounded
// are used for fine tuning the movements.
let playerAttackEnemy = (. o1, enemyTyp: Actors.enemyTyp, s2, o2, state: State.t) => {
  o1.Object.invuln = 10
  o1.jumping = false
  o1.grounded = true
  switch enemyTyp {
  | GKoopaShell | RKoopaShell =>
    let r2 = Object.evolveEnemy(. o1.dir, enemyTyp, s2, o2, state.level)
    o1.vy = -.Config.dampenJump
    o1.py = o1.py -. 5.
    (None, r2)
  | _ =>
    Object.decHealth(o2)
    o1.vy = -.Config.dampenJump
    if state.multiplier == 8 {
      state->State.updateScore(800)
      o2.score = 800
      (None, Object.evolveEnemy(. o1.dir, enemyTyp, s2, o2, state.level))
    } else {
      let score = 100 * state.multiplier
      state->State.updateScore(score)
      o2.score = score
      state.multiplier = state.multiplier * 2
      (None, Object.evolveEnemy(. o1.dir, enemyTyp, s2, o2, state.level))
    }
  }
}

// enemyAttackPlayer is used when an enemy kills a player.
let enemyAttackPlayer = (. enemy: Object.t, player: Object.t, level) => {
  switch enemy.objTyp {
  | Enemy((GKoopaShell | RKoopaShell) as enemyTyp) if enemy.vx == 0. =>
    // This only works if the player does not go faster than the shell
    // Otherwise it can try to overtake and touch it when it has non-zero velocity
    let r2 = {
      Object.evolveEnemy(. player.dir, enemyTyp, enemy.sprite, enemy, level)
    }
    (None, r2)
  | _ =>
    Object.decHealth(player)
    player.invuln = Config.invuln
    (None, None)
  }
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
let processCollision = (. dir: Actors.dir2d, obj: Object.t, collid: Object.t, state: State.t) =>
  switch (obj, collid, dir) {
  | ({objTyp: Player(_)}, {objTyp: Player(_)}, East | West) =>
    collid.vx = collid.vx +. obj.vx
    (None, None)
  | ({objTyp: Player(_)}, {objTyp: Enemy(typ), sprite: s2}, South)
  | ({objTyp: Enemy(typ), sprite: s2}, {objTyp: Player(_)}, North) =>
    playerAttackEnemy(. obj, typ, s2, collid, state)
  | ({objTyp: Player(_)}, {objTyp: Enemy(_)}, _) => enemyAttackPlayer(. collid, obj, state.level)
  | ({objTyp: Enemy(_)}, {objTyp: Player(_)}, _) => enemyAttackPlayer(. obj, collid, state.level)
  | ({objTyp: Player(_)}, {objTyp: Item(t2)}, _) | ({objTyp: Item(t2)}, {objTyp: Player(_)}, _) =>
    switch t2 {
    | Mushroom =>
      Object.decHealth(collid)
      if obj.health == 2 {
        ()
      } else {
        obj.health = obj.health + 1
      }
      obj.vx = 0.
      obj.vy = 0.
      state->State.updateScore(1000)
      collid.score = 1000
      (None, None)
    | Coin =>
      state.coins = state.coins + 1
      Object.decHealth(collid)
      state->State.updateScore(100)
      (None, None)
    }
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Enemy(t2), sprite: s2}, dir) =>
    collEnemyEnemy(t1, s1, obj, t2, s2, collid, dir)
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Block(t2)}, East)
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Block(t2)}, West) =>
    switch (t1, t2) {
    | (RKoopaShell, Brick) | (GKoopaShell, Brick) =>
      Object.decHealth(collid)
      Object.reverseLeftRight(obj)
      (None, None)
    | (RKoopaShell, QBlock(typ)) | (GKoopaShell, QBlock(typ)) =>
      let updatedBlock = Object.evolveBlock(. collid, state.level)
      let spawnedItem = Object.spawnAbove(. obj.dir, collid, typ, state.level)
      Object.revDir(obj, t1, s1)
      (Some(updatedBlock), Some(spawnedItem))
    | (_, _) =>
      Object.revDir(obj, t1, s1)
      (None, None)
    }
  | ({objTyp: Item(_)}, {objTyp: Block(_)}, East) | ({objTyp: Item(_)}, {objTyp: Block(_)}, West) =>
    Object.reverseLeftRight(obj)
    (None, None)
  | ({objTyp: Enemy(_)}, {objTyp: Block(_)}, _) | ({objTyp: Item(_)}, {objTyp: Block(_)}, _) =>
    Object.collideBlock(dir, obj)
    (None, None)
  | ({objTyp: Player(t1, _)}, {objTyp: Block(t)}, North) =>
    switch t {
    | QBlock(typ) =>
      let updatedBlock = Object.evolveBlock(. collid, state.level)
      let spawnedItem = Object.spawnAbove(. obj.dir, collid, typ, state.level)
      Object.collideBlock(dir, obj)
      (Some(spawnedItem), Some(updatedBlock))
    | Brick =>
      if t1 == BigM {
        Object.collideBlock(dir, obj)
        Object.decHealth(collid)
        (None, None)
      } else {
        Object.collideBlock(dir, obj)
        (None, None)
      }
    | Panel =>
      state.status = Finished({
        levelResult: Won,
        restartTime: Config.delayWhenFinished +. Html.performance.now(.),
      })
      (None, None)
    | _ =>
      Object.collideBlock(dir, obj)
      (None, None)
    }
  | ({objTyp: Player(_)}, {objTyp: Block(t)}, _) =>
    switch t {
    | Panel =>
      state.status = Finished({
        levelResult: Won,
        restartTime: Config.delayWhenFinished +. Html.performance.now(.),
      })
      (None, None)
    | _ =>
      switch dir {
      | South =>
        state.multiplier = 1
        Object.collideBlock(dir, obj)
        (None, None)
      | _ =>
        Object.collideBlock(dir, obj)
        (None, None)
      }
    }
  | (_, _, _) => (None, None)
  }

let inViewport = (obj: Object.t, ~viewport) =>
  Viewport.inViewport(viewport, obj.px, obj.py) ||
  (Object.isPlayer(obj) ||
  Viewport.outOfViewportBelow(viewport, obj.py))

// Run the broad phase object filtering
let broadPhase = (~allCollids, viewport) => allCollids->List.keep(o => o->inViewport(~viewport))

// narrowPhase of collision is used in order to continuously loop through
// each of the collidable objects to constantly check if collisions are
// occurring.
let narrowPhase = (obj, ~state, ~visibleCollids) => {
  let rec narrowHelper = (obj: Object.t, ~visibleCollids, ~acc) =>
    switch visibleCollids {
    | list{} => acc
    | list{collid, ...nextVisibleCollids} =>
      let newObjs = if !Object.sameId(obj, collid) {
        switch Object.checkCollision(obj, collid) {
        | None => (None, None)
        | Some(dir) => processCollision(. dir, obj, collid, state)
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
      narrowHelper(obj, ~visibleCollids=nextVisibleCollids, ~acc)
    }
  narrowHelper(obj, ~visibleCollids, ~acc=list{})
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
let checkCollisions = (obj, ~state: State.t, ~allCollids) =>
  switch obj.Object.objTyp {
  | Block(_) => list{}
  | _ =>
    let visibleCollids = broadPhase(~allCollids, state.viewport)
    obj->narrowPhase(~state, ~visibleCollids)
  }

// primary update method for objects,
// checking the collision, updating the object, and drawing to the canvas
let findObjectsColliding = (obj: Object.t, ~allCollids, ~state: State.t) => {
  /* TODO: optimize. Draw static elements only once */
  let sprite = obj.sprite
  obj.invuln = if obj.invuln > 0 {
    obj.invuln - 1
  } else {
    0
  }
  if (!obj.kill || obj->Object.isPlayer) && obj->inViewport(~viewport=state.viewport) {
    obj.grounded = false
    obj->Object.processObj(~level=state.level)
    // Run collision detection if moving object
    let objectsColliding = obj->checkCollisions(~state, ~allCollids)
    if obj.vx != 0. || !Object.isEnemy(obj) {
      Sprite.updateAnimation(sprite)
    }
    objectsColliding
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
  | Player(_, playerNum) =>
    let keys = Keys.translateKeys(playerNum)
    obj.crouch = false
    obj->Object.updatePlayer(playerNum, keys)
    let objectsColliding = obj->findObjectsColliding(~allCollids, ~state)
    state.objects = \"@"(objectsColliding, state.objects)
  | _ =>
    let objectsColliding = obj->findObjectsColliding(~allCollids, ~state)
    if !obj.kill {
      state.objects = list{obj, ...\"@"(objectsColliding, state.objects)}
    }
    let newParts = if obj.kill {
      Object.kill(obj)
    } else {
      list{}
    }
    state.particles = \"@"(newParts, state.particles)
  }

// Primary update function to update and persist a particle
let updateParticle = part => {
  Particle.process(part)
  !part.kill
}

type auth = LoggedOut | LoggedIn(Candid.principal)

let auth = ref(LoggedOut)

// updateLoop is constantly being called to check for collisions and to
// update each of the objects in the game.
let rec updateLoop = () => {
  let startLogin = (~onLogged) => {
    State.current.contents.status = LoggingIn
    AuthClient.authenticate(
      ~onSuccess=(~principal) => {
        auth := LoggedIn(principal)
        onLogged(~principal)
      },
      ~onError=error => {
        Js.log2("error", error->AuthClient.Error.toString)
        State.current.contents.status = Playing
      },
      ~timeoutInSeconds=30.,
    )->ignore
  }
  switch Keys.pressedKeys.pendingStateOperations {
  | Some(LoadState) =>
    Keys.pressedKeys.pendingStateOperations = None
    let doLoad = (~principal) => {
      Js.log("loading...")
      State.current.contents.status = Loading
      State.load(~principal)
      ->Promise.thenResolve(() => {
        Js.log("loaded")
        State.current.contents.status = Playing
      })
      ->ignore
    }
    switch auth.contents {
    | LoggedOut => startLogin(~onLogged=doLoad)
    | LoggedIn(principal) => doLoad(~principal)
    }
  | Some(SaveState) =>
    Keys.pressedKeys.pendingStateOperations = None
    let doSave = (~principal) => {
      Js.log("saving...")
      State.current.contents.status = Saving
      State.save(~principal)
      ->Promise.thenResolve(() => {
        Js.log("saved")
        State.current.contents.status = Playing
      })
      ->ignore
    }
    switch auth.contents {
    | LoggedOut => startLogin(~onLogged=doSave)
    | LoggedIn(principal) => doSave(~principal)
    }
  | None =>
    if Keys.pressedKeys.paused {
      State.current.contents.status = Paused
    } else if State.current.contents.status == Paused {
      State.current.contents.status = Playing
    }
  }

  switch State.current.contents.status {
  | LoggingIn =>
    State.current.contents->Draw.drawState(~fps=0.)
    Draw.loggingIn()
    Html.requestAnimationFrame(_ => updateLoop())

  | Loading =>
    State.current.contents->Draw.drawState(~fps=0.)
    Draw.loggingIn()
    Html.requestAnimationFrame(_ => updateLoop())

  | Saving =>
    State.current.contents->Draw.drawState(~fps=0.)
    Draw.saving()
    Html.requestAnimationFrame(_ => updateLoop())

  | Paused =>
    State.current.contents->Draw.drawState(~fps=0.)
    Draw.paused()
    Html.requestAnimationFrame(_ => updateLoop())

  | Finished({levelResult, restartTime}) =>
    let timeToStart = (restartTime -. Html.performance.now(.)) /. 1000.
    if timeToStart > 0.9 /* briefly show 0 */ {
      Draw.levelFinished(
        levelResult,
        State.current.contents.level->string_of_int,
        timeToStart->int_of_float->string_of_int,
      )
      Html.requestAnimationFrame(_ => updateLoop())
    } else {
      let level =
        levelResult == Won ? State.current.contents.level + 1 : State.current.contents.level
      let score = levelResult == Won ? State.current.contents.score : 0
      State.current := State.new(~level, ~score)
      updateLoop()
    }

  | Playing =>
    let fps = calcFps()
    let oldObjects = State.current.contents.objects
    State.current.contents.objects = list{}
    State.current.contents.particles = State.current.contents.particles->List.keep(updateParticle)
    State.current.contents.player1->updateObject(
      ~allCollids=Keys.checkTwoPlayers()
        ? list{State.current.contents.player2, ...oldObjects}
        : oldObjects,
      ~state=State.current.contents,
    )
    if Keys.checkTwoPlayers() {
      State.current.contents.player2->updateObject(
        ~allCollids=list{State.current.contents.player1, ...oldObjects},
        ~state=State.current.contents,
      )
    }
    if State.current.contents.player1.kill {
      State.current.contents.status = Finished({
        levelResult: Lost,
        restartTime: Config.delayWhenFinished +. Html.performance.now(.),
      })
    }
    Viewport.update(
      State.current.contents.viewport,
      State.current.contents.player1.px,
      State.current.contents.player1.py,
    )
    oldObjects->List.forEach(obj =>
      obj->updateObject(~allCollids=oldObjects, ~state=State.current.contents)
    )

    State.current.contents->Draw.drawState(~fps)
    Html.requestAnimationFrame(_ => updateLoop())
  }
}
