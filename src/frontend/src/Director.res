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
let playerAttackEnemy = (.
  o1: Types.obj,
  enemyTyp: Types.enemyTyp,
  s2,
  o2,
  state: Types.state,
  objects,
) => {
  o1.invuln = 10
  o1.jumping = false
  o1.grounded = true
  switch enemyTyp {
  | GKoopaShell | RKoopaShell =>
    Object.evolveEnemy(. o1.dir, enemyTyp, s2, o2, state.level, objects)
    o1.vy = -.Config.dampenJump
    o1.py = o1.py -. 5.
  | _ =>
    Object.decHealth(o2)
    o1.vy = -.Config.dampenJump
    if state.multiplier == 8 {
      state->State.updateScore(800)
      o2.score = 800
      Object.evolveEnemy(. o1.dir, enemyTyp, s2, o2, state.level, objects)
    } else {
      let score = 100 * state.multiplier
      state->State.updateScore(score)
      o2.score = score
      state.multiplier = state.multiplier * 2
      Object.evolveEnemy(. o1.dir, enemyTyp, s2, o2, state.level, objects)
    }
  }
}

// enemyAttackPlayer is used when an enemy kills a player.
let enemyAttackPlayer = (. enemy: Types.obj, player: Types.obj, level, objects) => {
  switch enemy.objTyp {
  | Enemy((GKoopaShell | RKoopaShell) as enemyTyp) if enemy.vx == 0. =>
    // This only works if the player does not go faster than the shell
    // Otherwise it can try to overtake and touch it when it has non-zero velocity
    Object.evolveEnemy(. player.dir, enemyTyp, enemy.sprite, enemy, level, objects)
  | _ =>
    Object.decHealth(player)
    player.invuln = Config.invuln
  }
}
// In the case that two enemies collide, they are to reverse directions. However,
// in the case that one or more of the two enemies is a koopa shell, then
// the koopa shell kills the other enemy.
let collEnemyEnemy = (.
  enemy1: Types.enemyTyp,
  s1,
  o1,
  enemy2: Types.enemyTyp,
  s2,
  o2,
  dir2: Types.dir2,
) =>
  switch (enemy1, enemy2) {
  | (GKoopaShell, GKoopaShell)
  | (GKoopaShell, RKoopaShell)
  | (RKoopaShell, RKoopaShell)
  | (RKoopaShell, GKoopaShell) =>
    Object.decHealth(o1)
    Object.decHealth(o2)
  | (RKoopaShell, _) | (GKoopaShell, _) =>
    if o1.vx == 0. {
      Object.revDir(o2, enemy2, s2)
    } else {
      Object.decHealth(o2)
    }
  | (_, RKoopaShell) | (_, GKoopaShell) =>
    if o2.vx == 0. {
      Object.revDir(o1, enemy1, s1)
    } else {
      Object.decHealth(o1)
    }
  | (_, _) =>
    switch dir2 {
    | West | East =>
      Object.revDir(o1, enemy1, s1)
      Object.revDir(o2, enemy2, s2)
    | _ => ()
    }
  }

module Global = {
  type status =
    | Loading
    | LoggingIn(Types.loadOrSave)
    | Paused
    | Playing
    | Finished({levelResult: Types.levelResult, restartTime: float})
    | Saving
  type initialObj = {obj: Types.obj, mutable missing: bool}
  type global = {
    idCounter: ref<int>,
    mutable state: Types.state,
    mutable status: status,
    mutable initialObjects: Hashtbl.t<int, initialObj>,
  }

  let createInitialObjects = objects => {
    let initialObjects = Hashtbl.create(objects->Array.length)
    objects->Array.forEach((obj: Types.obj) =>
      initialObjects->Hashtbl.replace(obj.id, {obj: obj->Object.copy, missing: true})
    )
    initialObjects
  }
  let global = () => {
    let idCounter = ref(0)
    let state = State.new(~idCounter, ~level=1, ~score=0)
    {
      idCounter: idCounter,
      state: state,
      status: Playing,
      initialObjects: state.objects->createInitialObjects,
    }
  }
  let reset = (global, ~level, ~score) => {
    global.idCounter := 0
    global.state = State.new(~idCounter=global.idCounter, ~level, ~score)
    global.status = Playing
    global.initialObjects = global.state.objects->createInitialObjects
  }
}

module Delta = {
  let apply = (delta: Types.delta, ~global: Global.global) => {
    if delta.state.level != global.state.level {
      global->Global.reset(~level=delta.state.level, ~score=delta.state.score)
    }
    let modifiedOrAdded = delta.state.objects
    let objects = []
    let addObject = obj => objects->Js.Array2.push(obj->Object.copy)->ignore

    global.initialObjects |> Hashtbl.iter((_id, initialObj: Global.initialObj) =>
      initialObj.missing = false
    )
    delta.missing->Js.Array2.forEach(id =>
      switch global.initialObjects->Hashtbl.find_opt(id) {
      | Some(initialObj) => initialObj.missing = true
      | None => ()
      }
    )

    modifiedOrAdded->Js.Array2.forEach(obj => {
      switch global.initialObjects->Hashtbl.find_opt(obj.id) {
      | Some(initialObj) =>
        // Prevent later adding an object with the same id from the initial objects
        initialObj.missing = true
      | None => ()
      }
      obj->addObject
    })

    global.initialObjects |> Hashtbl.iter((_id, initialObj: Global.initialObj) =>
      if initialObj.missing == false {
        initialObj.missing = true
        initialObj.obj->addObject
      }
    )

    let state = {...delta.state, objects: objects}
    global.state = state
  }

  let findObjectsDifference = (global: Global.global) => {
    let missing = []
    let modifiedOrAdded = []
    global.state.objects->Array.forEach(obj => {
      switch global.initialObjects->Hashtbl.find_opt(obj.id) {
      | Some(initialObj) =>
        initialObj.missing = false
        let isSame = initialObj.obj == obj
        if !isSame {
          // modified
          modifiedOrAdded->Js.Array2.push(obj)->ignore
        }
      | None =>
        // added
        modifiedOrAdded->Js.Array2.push(obj)->ignore
      }
    })
    global.initialObjects |> Hashtbl.iter((id, initialObj: Global.initialObj) => {
      if initialObj.missing {
        missing->Js.Array2.push(id)->ignore
      } else {
        initialObj.missing = true
      }
    })
    let delta: Types.delta = {
      missing: missing,
      state: {...global.state, objects: modifiedOrAdded},
    }
    delta
  }
}

let global = Global.global()

let loadState = (~principal) => {
  Backend.actor.loadGameState(. principal)->Promise.then(json => {
    if json != "" {
      global.state = json->Js.Json.parseExn->Obj.magic
    }
    Promise.resolve()
  })
}

let saveState = (~principal) =>
  Backend.actor.saveGameState(. principal, global.state->Obj.magic->Js.Json.stringify)

let loadStateBinary = (~principal) => {
  Backend.actor.loadGameStateNative(. principal)->Promise.then(arr => {
    switch arr {
    | [delta] => delta->Delta.apply(~global)
    | _ => ()
    }
    Promise.resolve()
  })
}

let saveStateBinary = (~principal, ~delta) => Backend.actor.saveGameStateNative(. principal, delta)

// Process collision is called to match each of the possible collisions that
// may occur. Returns a pair of options, representing objects that
// were created from the existing ones. That is, the first element represents
// a new item spawned as a result of the first object. None indicates that
// no new item should be spawned. Transformations to existing objects occur
// mutably, as many changes are side-effectual.
let processCollision = (.
  dir2: Types.dir2,
  obj: Types.obj,
  collid: Types.obj,
  idCounter,
  state: Types.state,
  objects: Js.Array2.t<Types.obj>,
) =>
  switch (obj, collid, dir2) {
  | ({objTyp: Player1(_) | Player2(_)}, {objTyp: Player1(_) | Player2(_)}, East | West) =>
    collid.vx = collid.vx +. obj.vx
  | ({objTyp: Player1(_) | Player2(_)}, {objTyp: Enemy(typ), sprite: s2}, South)
  | ({objTyp: Enemy(typ), sprite: s2}, {objTyp: Player1(_) | Player2(_)}, North) =>
    playerAttackEnemy(. obj, typ, s2, collid, state, objects)
  | ({objTyp: Player1(_) | Player2(_)}, {objTyp: Enemy(_)}, _) =>
    enemyAttackPlayer(. collid, obj, state.level, objects)
  | ({objTyp: Enemy(_)}, {objTyp: Player1(_) | Player2(_)}, _) =>
    enemyAttackPlayer(. obj, collid, state.level, objects)
  | ({objTyp: Player1(_) | Player2(_)}, {objTyp: Item(t2)}, _)
  | ({objTyp: Item(t2)}, {objTyp: Player1(_) | Player2(_)}, _) =>
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
    | Coin =>
      state.coins = state.coins + 1
      Object.decHealth(collid)
      state->State.updateScore(100)
    }
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Enemy(t2), sprite: s2}, dir) =>
    collEnemyEnemy(. t1, s1, obj, t2, s2, collid, dir)
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Block(t2)}, East)
  | ({objTyp: Enemy(t1), sprite: s1}, {objTyp: Block(t2)}, West) =>
    switch (t1, t2) {
    | (RKoopaShell, Brick) | (GKoopaShell, Brick) =>
      Object.decHealth(collid)
      Object.reverseLeftRight(obj)
    | (RKoopaShell | GKoopaShell, QBlockMushroom) =>
      Object.evolveBlock(. collid, idCounter, state.level, objects)
      Object.spawnAbove(. obj.dir, collid, Mushroom, state.level, objects)
      Object.revDir(obj, t1, s1)
    | (RKoopaShell | GKoopaShell, QBlockCoin) =>
      Object.evolveBlock(. collid, idCounter, state.level, objects)
      Object.spawnAbove(. obj.dir, collid, Coin, state.level, objects)
      Object.revDir(obj, t1, s1)
    | (_, _) => Object.revDir(obj, t1, s1)
    }
  | ({objTyp: Item(_)}, {objTyp: Block(_)}, East) | ({objTyp: Item(_)}, {objTyp: Block(_)}, West) =>
    Object.reverseLeftRight(obj)
  | ({objTyp: Enemy(_)}, {objTyp: Block(_)}, _) | ({objTyp: Item(_)}, {objTyp: Block(_)}, _) =>
    Object.collideBlock(dir2, obj)
  | ({objTyp: Player1(t1) | Player2(t1)}, {objTyp: Block(t)}, North) =>
    switch t {
    | QBlockMushroom =>
      Object.evolveBlock(. collid, idCounter, state.level, objects)
      Object.spawnAbove(. obj.dir, collid, Mushroom, state.level, objects)
      Object.collideBlock(dir2, obj)
    | QBlockCoin =>
      Object.evolveBlock(. collid, idCounter, state.level, objects)
      Object.spawnAbove(. obj.dir, collid, Coin, state.level, objects)
      Object.collideBlock(dir2, obj)
    | Brick =>
      if t1 == BigM {
        Object.collideBlock(dir2, obj)
        Object.decHealth(collid)
      } else {
        Object.collideBlock(dir2, obj)
      }
    | Panel =>
      global.status = Finished({
        levelResult: Won,
        restartTime: Config.delayWhenFinished +. Html.performance.now(.),
      })
    | _ => Object.collideBlock(dir2, obj)
    }
  | ({objTyp: Player1(_) | Player2(_)}, {objTyp: Block(t)}, _) =>
    switch t {
    | Panel =>
      global.status = Finished({
        levelResult: Won,
        restartTime: Config.delayWhenFinished +. Html.performance.now(.),
      })
    | _ =>
      switch dir2 {
      | South =>
        state.multiplier = 1
        Object.collideBlock(dir2, obj)
      | _ => Object.collideBlock(dir2, obj)
      }
    }
  | (_, _, _) => ()
  }

let inViewport = (obj: Types.obj, ~viewport) =>
  Viewport.inViewport(viewport, obj.px, obj.py) ||
  (Object.isPlayer(obj) ||
  Viewport.outOfViewportBelow(viewport, obj.py))

// Run the broad phase object filtering
let broadPhase = (~objects, viewport) => objects->Array.keep(o => o->inViewport(~viewport))

// narrowPhase of collision is used in order to continuously loop through
// each of the collidable objects to constantly check if collisions are
// occurring.
let narrowPhase = (obj, ~idCounter, ~objects, ~state, ~collids) => {
  collids->Js.Array2.forEach(collid =>
    if !Object.sameId(obj, collid) {
      switch Object.checkCollision(obj, collid) {
      | None => ()
      | Some(dir) => processCollision(. dir, obj, collid, idCounter, state, objects)
      }
    }
  )
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
let checkCollisions = (
  obj: Types.obj,
  ~idCounter,
  ~objects,
  ~otherCollids,
  ~state: Types.state,
  ~visibleCollids,
) =>
  switch obj.objTyp {
  | Block(_) => ()
  | _ =>
    obj->narrowPhase(~idCounter, ~objects, ~state, ~collids=visibleCollids)
    obj->narrowPhase(~idCounter, ~objects, ~state, ~collids=otherCollids)
  }

// primary update method for objects,
// checking the collision, updating the object, and drawing to the canvas
let findObjectsColliding = (
  obj: Types.obj,
  ~idCounter,
  ~objects,
  ~otherCollids,
  ~state: Types.state,
  ~visibleCollids,
) => {
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
    let objectsColliding =
      obj->checkCollisions(~idCounter, ~objects, ~otherCollids, ~state, ~visibleCollids)
    if obj.vx != 0. || !Object.isEnemy(obj) {
      Sprite.updateAnimation(sprite)
    }
    objectsColliding
  }
}

// used to update all of the objects at once. Primarily used
// as a wrapper method. This method is necessary to differentiate between
// the player collidable and the remaining collidables, as special operations
// such as viewport centering only occur with the player
let updateObject = (obj: Types.obj, ~idCounter, ~objects, ~otherCollids, ~state, ~visibleCollids) =>
  switch obj.objTyp {
  | Player1(_) | Player2(_) =>
    let playerNum: Types.playerNum = switch obj.objTyp {
    | Player1(_) => One
    | _ => Two
    }
    let keys = Keys.translateKeys(playerNum)
    obj.crouch = false
    obj->Object.updatePlayer(playerNum, keys)
    obj->findObjectsColliding(~idCounter, ~objects, ~otherCollids, ~state, ~visibleCollids)
  | _ =>
    obj->findObjectsColliding(~idCounter, ~objects, ~otherCollids, ~state, ~visibleCollids)
    if !obj.kill {
      global.state.objects->Js.Array2.push(obj)->ignore
    }
    if obj.kill {
      obj->Object.kill(~state)
    }
  }

// Primary update function to update and persist a particle
let updateParticle = part => {
  Particle.process(part)
  !part.kill
}

type auth = LoggedOut | LoggedIn(Backend.Candid.principal)

let auth = ref(LoggedOut)

// updateLoop is constantly being called to check for collisions and to
// update each of the objects in the game.
let rec updateLoop = () => {
  let startLogin = (~onLogged, ~loadOrSave) => {
    global.status = LoggingIn(loadOrSave)
    AuthClient.authenticate(
      ~onSuccess=(~principal) => {
        auth := LoggedIn(principal)
        onLogged(~principal)
      },
      ~onError=error => {
        Js.log2("error", error->AuthClient.Error.toString)
        global.status = Playing
      },
      ~timeoutInSeconds=30.,
    )->ignore
  }
  switch Keys.pressedKeys.pendingStateOperations {
  | Some(Load) =>
    Keys.pressedKeys.pendingStateOperations = None
    let doLoad = (~principal) => {
      Js.log("loading...")
      global.status = Loading
      loadStateBinary(~principal)
      ->Promise.thenResolve(() => {
        Js.log("loaded")
        global.status = Playing
      })
      ->ignore
    }
    switch auth.contents {
    | LoggedOut => startLogin(~onLogged=doLoad, ~loadOrSave=Load)
    | LoggedIn(principal) => doLoad(~principal)
    }
  | Some(Save) =>
    Keys.pressedKeys.pendingStateOperations = None
    let doSave = (~principal, ~delta: Types.delta) => {
      Js.log("saving...")
      global.status = Saving
      saveStateBinary(~principal, ~delta)
      ->Promise.thenResolve(() => {
        Js.log("saved")
        global.status = Playing
      })
      ->ignore
    }

    let delta = global->Delta.findObjectsDifference

    switch auth.contents {
    | LoggedOut =>
      startLogin(~onLogged=(~principal) => doSave(~principal, ~delta), ~loadOrSave=Save)
    | LoggedIn(principal) => doSave(~principal, ~delta)
    }
  | None =>
    if Keys.pressedKeys.paused {
      global.status = Paused
    } else if global.status == Paused {
      global.status = Playing
    }
  }

  switch global.status {
  | LoggingIn(loadOrSave) =>
    global.state->Draw.drawState(~fps=0.)
    Draw.loggingIn(~loadOrSave)
    Html.requestAnimationFrame(_ => updateLoop())

  | Loading =>
    global.state->Draw.drawState(~fps=0.)
    Draw.loading()
    Html.requestAnimationFrame(_ => updateLoop())

  | Saving =>
    global.state->Draw.drawState(~fps=0.)
    Draw.saving()
    Html.requestAnimationFrame(_ => updateLoop())

  | Paused =>
    global.state->Draw.drawState(~fps=0.)
    Draw.paused()
    Html.requestAnimationFrame(_ => updateLoop())

  | Finished({levelResult, restartTime}) =>
    let timeToStart = (restartTime -. Html.performance.now(.)) /. 1000.
    if timeToStart > 0.9 /* briefly show 0 */ {
      Draw.levelFinished(
        levelResult,
        global.state.level->string_of_int,
        timeToStart->int_of_float->string_of_int,
      )
      Html.requestAnimationFrame(_ => updateLoop())
    } else {
      let level = levelResult == Won ? global.state.level + 1 : global.state.level
      let score = levelResult == Won ? global.state.score : 0
      global->Global.reset(~level, ~score)
      updateLoop()
    }

  | Playing =>
    let fps = calcFps()
    let oldObjects = global.state.objects
    let visibleCollids = broadPhase(~objects=oldObjects, global.state.viewport)
    global.state.objects = []
    global.state.particles = global.state.particles->Belt.Array.keep(updateParticle)
    global.state.player1->updateObject(
      ~idCounter=global.idCounter,
      ~objects=global.state.objects,
      ~otherCollids=Keys.checkTwoPlayers() ? [global.state.player2] : [],
      ~state=global.state,
      ~visibleCollids,
    )
    if Keys.checkTwoPlayers() {
      global.state.player2->updateObject(
        ~idCounter=global.idCounter,
        ~objects=global.state.objects,
        ~otherCollids=[global.state.player1],
        ~state=global.state,
        ~visibleCollids,
      )
    }
    if global.state.player1.kill {
      global.status = Finished({
        levelResult: Lost,
        restartTime: Config.delayWhenFinished +. Html.performance.now(.),
      })
    }
    Viewport.update(global.state.viewport, global.state.player1.px, global.state.player1.py)
    oldObjects->Js.Array2.forEach(obj =>
      obj->updateObject(
        ~idCounter=global.idCounter,
        ~objects=global.state.objects,
        ~otherCollids=[],
        ~state=global.state,
        ~visibleCollids,
      )
    )

    global.state->Draw.drawState(~fps)
    Html.requestAnimationFrame(_ => updateLoop())
  }
}
