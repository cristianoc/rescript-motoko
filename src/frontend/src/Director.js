// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Draw from "./Draw.js";
import * as Keys from "./Keys.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as State from "./State.js";
import * as Config from "./Config.js";
import * as $$Object from "./Object.js";
import * as Sprite from "./Sprite.js";
import * as Backend from "./Backend.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Particle from "./Particle.js";
import * as Viewport from "./Viewport.js";
import * as AuthClient from "./AuthClient.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";

var lastTime = {
  contents: 0
};

var initialTime = {
  contents: 0
};

function calcFps(param) {
  var t0 = lastTime.contents;
  var time = performance.now();
  lastTime.contents = time;
  if (t0 === 0) {
    initialTime.contents = time;
    return 0;
  }
  var delta = (time - t0) / 1000;
  if (time - initialTime.contents < 1000.0) {
    return 0;
  } else {
    return 1 / delta;
  }
}

function playerAttackEnemy(o1, enemyTyp, s2, o2, state) {
  o1.invuln = 10;
  o1.jumping = false;
  o1.grounded = true;
  if (enemyTyp >= 3) {
    $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2, state);
    o1.vy = -Config.dampenJump;
    o1.py = o1.py - 5;
    return ;
  }
  $$Object.decHealth(o2);
  o1.vy = -Config.dampenJump;
  if (state.multiplier === 8) {
    State.updateScore(state, 800);
    o2.score = 800;
    return $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2, state);
  }
  var score = Math.imul(100, state.multiplier);
  State.updateScore(state, score);
  o2.score = score;
  state.multiplier = (state.multiplier << 1);
  return $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2, state);
}

function enemyAttackPlayer(enemy, player, state) {
  var enemyTyp = enemy.objTyp;
  if (enemyTyp.TAG === /* Enemy */2) {
    var enemyTyp$1 = enemyTyp._0;
    if (enemyTyp$1 >= 3 && enemy.vx === 0) {
      return $$Object.evolveEnemy(player.dir, enemyTyp$1, enemy.sprite, enemy, state);
    }
    
  }
  $$Object.decHealth(player);
  player.invuln = Config.invuln;
  
}

function collEnemyEnemy(enemy1, s1, o1, enemy2, s2, o2, dir2) {
  if (enemy1 !== 3) {
    if (enemy1 < 4) {
      if (enemy2 >= 3) {
        if (o2.vx === 0) {
          return $$Object.revDir(o1, enemy1, s1);
        } else {
          return $$Object.decHealth(o1);
        }
      } else if (dir2 >= 2) {
        $$Object.revDir(o1, enemy1, s1);
        return $$Object.revDir(o2, enemy2, s2);
      } else {
        return ;
      }
    }
    if (enemy2 >= 3) {
      $$Object.decHealth(o1);
      return $$Object.decHealth(o2);
    }
    
  } else if (enemy2 >= 3) {
    $$Object.decHealth(o1);
    return $$Object.decHealth(o2);
  }
  if (o1.vx === 0) {
    return $$Object.revDir(o2, enemy2, s2);
  } else {
    return $$Object.decHealth(o2);
  }
}

function createInitialObjects(objects) {
  var initialObjects = Hashtbl.create(undefined, objects.length);
  Belt_Array.forEach(objects, (function (obj) {
          return Hashtbl.replace(initialObjects, obj.id, {
                      obj: $$Object.copy(obj),
                      missing: true
                    });
        }));
  return initialObjects;
}

function $$global(param) {
  var state = State.$$new(1, 0);
  return {
          state: state,
          status: /* Playing */2,
          initialObjects: createInitialObjects(state.objects)
        };
}

function reset($$global, level, score) {
  $$global.state = State.$$new(level, score);
  $$global.status = /* Playing */2;
  $$global.initialObjects = createInitialObjects($$global.state.objects);
  
}

var Global = {
  createInitialObjects: createInitialObjects,
  $$global: $$global,
  reset: reset
};

function apply(delta, $$global) {
  if (delta.state.level !== $$global.state.level) {
    reset($$global, delta.state.level, delta.state.score);
  }
  var modifiedOrAdded = delta.state.objects;
  var objects = [];
  var addObject = function (obj) {
    objects.push($$Object.copy(obj));
    
  };
  Hashtbl.iter((function (_id, initialObj) {
          initialObj.missing = false;
          
        }), $$global.initialObjects);
  delta.missing.forEach(function (id) {
        var initialObj = Hashtbl.find_opt($$global.initialObjects, id);
        if (initialObj !== undefined) {
          initialObj.missing = true;
          return ;
        }
        
      });
  modifiedOrAdded.forEach(function (obj) {
        var initialObj = Hashtbl.find_opt($$global.initialObjects, obj.id);
        if (initialObj !== undefined) {
          initialObj.missing = true;
        }
        return addObject(obj);
      });
  Hashtbl.iter((function (_id, initialObj) {
          if (initialObj.missing === false) {
            initialObj.missing = true;
            return addObject(initialObj.obj);
          }
          
        }), $$global.initialObjects);
  var init = delta.state;
  var state = {
    bgd: init.bgd,
    coins: init.coins,
    idCounter: init.idCounter,
    level: init.level,
    multiplier: init.multiplier,
    objects: objects,
    particles: init.particles,
    player1: init.player1,
    player2: init.player2,
    score: init.score,
    viewport: init.viewport
  };
  $$global.state = state;
  
}

function findObjectsDifference($$global) {
  var missing = [];
  var modifiedOrAdded = [];
  Belt_Array.forEach($$global.state.objects, (function (obj) {
          var initialObj = Hashtbl.find_opt($$global.initialObjects, obj.id);
          if (initialObj !== undefined) {
            initialObj.missing = false;
            var isSame = Caml_obj.caml_equal(initialObj.obj, obj);
            if (!isSame) {
              modifiedOrAdded.push(obj);
              return ;
            } else {
              return ;
            }
          }
          modifiedOrAdded.push(obj);
          
        }));
  Hashtbl.iter((function (id, initialObj) {
          if (initialObj.missing) {
            missing.push(id);
          } else {
            initialObj.missing = true;
          }
          
        }), $$global.initialObjects);
  var init = $$global.state;
  return {
          missing: missing,
          state: {
            bgd: init.bgd,
            coins: init.coins,
            idCounter: init.idCounter,
            level: init.level,
            multiplier: init.multiplier,
            objects: modifiedOrAdded,
            particles: init.particles,
            player1: init.player1,
            player2: init.player2,
            score: init.score,
            viewport: init.viewport
          }
        };
}

var Delta = {
  apply: apply,
  findObjectsDifference: findObjectsDifference
};

var $$global$1 = $$global(undefined);

function loadDelta(principal) {
  return Backend.actor.loadDelta(principal).then(function (arr) {
              if (arr.length === 1) {
                var delta = arr[0];
                apply(delta, $$global$1);
              }
              return Promise.resolve(undefined);
            });
}

function saveDelta(principal, delta) {
  return Backend.actor.saveDelta(principal, delta);
}

function processCollision(dir2, obj, collid, state) {
  var exit = 0;
  var typ;
  var s2;
  var t2;
  var t1 = obj.objTyp;
  var exit$1 = 0;
  switch (t1.TAG | 0) {
    case /* Player1 */0 :
    case /* Player2 */1 :
        exit$1 = 3;
        break;
    case /* Enemy */2 :
        var t1$1 = t1._0;
        var s1 = obj.sprite;
        var t2$1 = collid.objTyp;
        var exit$2 = 0;
        switch (t2$1.TAG | 0) {
          case /* Player1 */0 :
          case /* Player2 */1 :
              exit$2 = 4;
              break;
          case /* Enemy */2 :
              var s2$1 = collid.sprite;
              return collEnemyEnemy(t1$1, s1, obj, t2$1._0, s2$1, collid, dir2);
          case /* Item */3 :
              return ;
          case /* Block */4 :
              var t2$2 = t2$1._0;
              if (dir2 < 2) {
                return $$Object.collideBlock(dir2, obj);
              }
              var exit$3 = 0;
              if (t1$1 !== 3) {
                if (t1$1 < 4) {
                  return $$Object.revDir(obj, t1$1, s1);
                }
                if (t2$2 !== 3) {
                  exit$3 = 5;
                } else {
                  $$Object.decHealth(collid);
                  return $$Object.reverseLeftRight(obj);
                }
              } else if (t2$2 !== 3) {
                exit$3 = 5;
              } else {
                $$Object.decHealth(collid);
                return $$Object.reverseLeftRight(obj);
              }
              if (exit$3 === 5) {
                if (t2$2 !== 1) {
                  if (t2$2 !== 0) {
                    return $$Object.revDir(obj, t1$1, s1);
                  } else {
                    $$Object.evolveBlock(collid, state);
                    $$Object.spawnAbove(obj.dir, collid, /* Mushroom */0, state);
                    return $$Object.revDir(obj, t1$1, s1);
                  }
                } else {
                  $$Object.evolveBlock(collid, state);
                  $$Object.spawnAbove(obj.dir, collid, /* Coin */1, state);
                  return $$Object.revDir(obj, t1$1, s1);
                }
              }
              break;
          
        }
        if (exit$2 === 4) {
          if (dir2 !== 0) {
            return enemyAttackPlayer(obj, collid, state);
          }
          typ = t1$1;
          s2 = s1;
          exit = 1;
        }
        break;
    case /* Item */3 :
        var match = collid.objTyp;
        switch (match.TAG | 0) {
          case /* Player1 */0 :
          case /* Player2 */1 :
              t2 = t1._0;
              exit = 2;
              break;
          case /* Enemy */2 :
          case /* Item */3 :
              return ;
          case /* Block */4 :
              if (dir2 >= 2) {
                return $$Object.reverseLeftRight(obj);
              } else {
                return $$Object.collideBlock(dir2, obj);
              }
          
        }
        break;
    case /* Block */4 :
        return ;
    
  }
  if (exit$1 === 3) {
    var typ$1 = collid.objTyp;
    var exit$4 = 0;
    switch (typ$1.TAG | 0) {
      case /* Player1 */0 :
      case /* Player2 */1 :
          exit$4 = 4;
          break;
      case /* Enemy */2 :
          var s2$2 = collid.sprite;
          if (dir2 !== 1) {
            return enemyAttackPlayer(collid, obj, state);
          }
          typ = typ$1._0;
          s2 = s2$2;
          exit = 1;
          break;
      case /* Item */3 :
          t2 = typ$1._0;
          exit = 2;
          break;
      case /* Block */4 :
          var exit$5 = 0;
          var t1$2;
          t1$2 = t1._0;
          exit$5 = 5;
          if (exit$5 === 5) {
            var t = collid.objTyp;
            if (t.TAG !== /* Block */4) {
              return ;
            }
            if (dir2 !== 0) {
              var exit$6 = 0;
              exit$6 = 6;
              if (exit$6 === 6) {
                var t$1 = collid.objTyp;
                if (t$1.TAG === /* Block */4) {
                  if (t$1._0 !== 6) {
                    if (dir2 !== 1) {
                      return $$Object.collideBlock(dir2, obj);
                    } else {
                      state.multiplier = 1;
                      return $$Object.collideBlock(dir2, obj);
                    }
                  } else {
                    $$global$1.status = {
                      TAG: /* Finished */1,
                      levelResult: /* Won */0,
                      restartTime: Config.delayWhenFinished + performance.now()
                    };
                    return ;
                  }
                } else {
                  return ;
                }
              }
              
            } else {
              switch (t._0) {
                case /* QBlockMushroom */0 :
                    $$Object.evolveBlock(collid, state);
                    $$Object.spawnAbove(obj.dir, collid, /* Mushroom */0, state);
                    return $$Object.collideBlock(dir2, obj);
                case /* QBlockCoin */1 :
                    $$Object.evolveBlock(collid, state);
                    $$Object.spawnAbove(obj.dir, collid, /* Coin */1, state);
                    return $$Object.collideBlock(dir2, obj);
                case /* Brick */3 :
                    if (t1$2 === /* BigM */0) {
                      $$Object.collideBlock(dir2, obj);
                      return $$Object.decHealth(collid);
                    } else {
                      return $$Object.collideBlock(dir2, obj);
                    }
                case /* Panel */6 :
                    $$global$1.status = {
                      TAG: /* Finished */1,
                      levelResult: /* Won */0,
                      restartTime: Config.delayWhenFinished + performance.now()
                    };
                    return ;
                case /* QBlockUsed */2 :
                case /* UnBBlock */4 :
                case /* Cloud */5 :
                case /* Ground */7 :
                    return $$Object.collideBlock(dir2, obj);
                
              }
            }
          }
          break;
      
    }
    if (exit$4 === 4) {
      if (dir2 >= 2) {
        collid.vx = collid.vx + obj.vx;
        return ;
      } else {
        return ;
      }
    }
    
  }
  switch (exit) {
    case 1 :
        return playerAttackEnemy(obj, typ, s2, collid, state);
    case 2 :
        if (t2) {
          state.coins = state.coins + 1 | 0;
          $$Object.decHealth(collid);
          return State.updateScore(state, 100);
        } else {
          $$Object.decHealth(collid);
          if (obj.health === 2) {
            
          } else {
            obj.health = obj.health + 1 | 0;
          }
          obj.vx = 0;
          obj.vy = 0;
          State.updateScore(state, 1000);
          collid.score = 1000;
          return ;
        }
    
  }
}

function inViewport(obj, viewport) {
  if (Viewport.inViewport(viewport, obj.px, obj.py) || $$Object.isPlayer(obj)) {
    return true;
  } else {
    return Viewport.outOfViewportBelow(viewport, obj.py);
  }
}

function broadPhase(objects, viewport) {
  return Belt_Array.keep(objects, (function (o) {
                return inViewport(o, viewport);
              }));
}

function narrowPhase(obj, state, collids) {
  collids.forEach(function (collid) {
        if ($$Object.sameId(obj, collid)) {
          return ;
        }
        var dir = $$Object.checkCollision(obj, collid);
        if (dir !== undefined) {
          return processCollision(dir, obj, collid, state);
        }
        
      });
  
}

function checkCollisions(obj, otherCollids, state, visibleCollids) {
  var match = obj.objTyp;
  if (match.TAG === /* Block */4) {
    return ;
  }
  narrowPhase(obj, state, visibleCollids);
  return narrowPhase(obj, state, otherCollids);
}

function findObjectsColliding(obj, otherCollids, state, visibleCollids) {
  var sprite = obj.sprite;
  obj.invuln = obj.invuln > 0 ? obj.invuln - 1 | 0 : 0;
  if (!((!obj.kill || $$Object.isPlayer(obj)) && inViewport(obj, state.viewport))) {
    return ;
  }
  obj.grounded = false;
  $$Object.processObj(obj, state.level);
  var objectsColliding = checkCollisions(obj, otherCollids, state, visibleCollids);
  if (obj.vx !== 0 || !$$Object.isEnemy(obj)) {
    Sprite.updateAnimation(sprite);
  }
  return objectsColliding;
}

function updateObject(obj, otherCollids, state, visibleCollids) {
  var match = obj.objTyp;
  switch (match.TAG | 0) {
    case /* Player1 */0 :
    case /* Player2 */1 :
        break;
    default:
      findObjectsColliding(obj, otherCollids, state, visibleCollids);
      if (!obj.kill) {
        $$global$1.state.objects.push(obj);
      }
      if (obj.kill) {
        return $$Object.kill(obj, state);
      } else {
        return ;
      }
  }
  var match$1 = obj.objTyp;
  var playerNum;
  playerNum = match$1.TAG === /* Player1 */0 ? /* One */0 : /* Two */1;
  var keys = Keys.translateKeys(playerNum);
  obj.crouch = false;
  $$Object.updatePlayer(obj, playerNum, keys);
  return findObjectsColliding(obj, otherCollids, state, visibleCollids);
}

function updateParticle(part) {
  Particle.$$process(part);
  return !part.kill;
}

var auth = {
  contents: /* LoggedOut */0
};

function updateLoop(_param) {
  while(true) {
    var startLogin = function (onLogged, loadOrSave) {
      $$global$1.status = {
        TAG: /* LoggingIn */0,
        _0: loadOrSave
      };
      AuthClient.authenticate((function (principal) {
              auth.contents = /* LoggedIn */{
                _0: principal
              };
              return Curry._1(onLogged, principal);
            }), (function (error) {
              console.log("error", AuthClient.$$Error.toString(error));
              $$global$1.status = /* Playing */2;
              
            }), 30);
      
    };
    var match = Keys.pressedKeys.pendingStateOperations;
    if (match !== undefined) {
      Keys.pressedKeys.pendingStateOperations = undefined;
      if (match) {
        var doSave = function (principal, delta) {
          console.log("saving...");
          $$global$1.status = /* Saving */3;
          saveDelta(principal, delta).then(function (param) {
                console.log("saved");
                $$global$1.status = /* Playing */2;
                
              });
          
        };
        var delta = findObjectsDifference($$global$1);
        var principal = auth.contents;
        if (principal) {
          doSave(principal._0, delta);
        } else {
          startLogin((function(delta){
              return function (principal) {
                return doSave(principal, delta);
              }
              }(delta)), /* Save */1);
        }
      } else {
        var doLoad = function (principal) {
          console.log("loading...");
          $$global$1.status = /* Loading */0;
          loadDelta(principal).then(function (param) {
                console.log("loaded");
                $$global$1.status = /* Playing */2;
                
              });
          
        };
        var principal$1 = auth.contents;
        if (principal$1) {
          doLoad(principal$1._0);
        } else {
          startLogin(doLoad, /* Load */0);
        }
      }
    } else if (Keys.pressedKeys.paused) {
      $$global$1.status = /* Paused */1;
    } else if ($$global$1.status === /* Paused */1) {
      $$global$1.status = /* Playing */2;
    }
    var loadOrSave = $$global$1.status;
    if (typeof loadOrSave === "number") {
      switch (loadOrSave) {
        case /* Loading */0 :
            Draw.drawState($$global$1.state, 0);
            Draw.loading(undefined);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        case /* Paused */1 :
            Draw.drawState($$global$1.state, 0);
            Draw.paused(undefined);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        case /* Playing */2 :
            var fps = calcFps(undefined);
            var oldObjects = $$global$1.state.objects;
            var visibleCollids = broadPhase(oldObjects, $$global$1.state.viewport);
            $$global$1.state.objects = [];
            $$global$1.state.particles = Belt_Array.keep($$global$1.state.particles, updateParticle);
            updateObject($$global$1.state.player1, Keys.checkTwoPlayers(undefined) ? [$$global$1.state.player2] : [], $$global$1.state, visibleCollids);
            if (Keys.checkTwoPlayers(undefined)) {
              updateObject($$global$1.state.player2, [$$global$1.state.player1], $$global$1.state, visibleCollids);
            }
            if ($$global$1.state.player1.kill) {
              $$global$1.status = {
                TAG: /* Finished */1,
                levelResult: /* Lost */1,
                restartTime: Config.delayWhenFinished + performance.now()
              };
            }
            Viewport.update($$global$1.state.viewport, $$global$1.state.player1.px, $$global$1.state.player1.py);
            oldObjects.forEach((function(visibleCollids){
                return function (obj) {
                  return updateObject(obj, [], $$global$1.state, visibleCollids);
                }
                }(visibleCollids)));
            Draw.drawState($$global$1.state, fps);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        case /* Saving */3 :
            Draw.drawState($$global$1.state, 0);
            Draw.saving(undefined);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        
      }
    } else {
      if (loadOrSave.TAG === /* LoggingIn */0) {
        Draw.drawState($$global$1.state, 0);
        Draw.loggingIn(loadOrSave._0);
        requestAnimationFrame(function (param) {
              return updateLoop(undefined);
            });
        return ;
      }
      var levelResult = loadOrSave.levelResult;
      var timeToStart = (loadOrSave.restartTime - performance.now()) / 1000;
      if (timeToStart > 0.9) {
        Draw.levelFinished(levelResult, String($$global$1.state.level), String(timeToStart | 0));
        requestAnimationFrame(function (param) {
              return updateLoop(undefined);
            });
        return ;
      }
      var level = levelResult === /* Won */0 ? $$global$1.state.level + 1 | 0 : $$global$1.state.level;
      var score = levelResult === /* Won */0 ? $$global$1.state.score : 0;
      reset($$global$1, level, score);
      _param = undefined;
      continue ;
    }
  };
}

export {
  calcFps ,
  playerAttackEnemy ,
  enemyAttackPlayer ,
  collEnemyEnemy ,
  Global ,
  Delta ,
  $$global$1 as $$global,
  loadDelta ,
  saveDelta ,
  processCollision ,
  inViewport ,
  broadPhase ,
  narrowPhase ,
  checkCollisions ,
  findObjectsColliding ,
  updateObject ,
  updateParticle ,
  auth ,
  updateLoop ,
  
}
/* global Not a pure module */
