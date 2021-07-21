// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Draw from "./Draw.js";
import * as Keys from "./Keys.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as State from "./State.js";
import * as Config from "./Config.js";
import * as $$Object from "./Object.js";
import * as Sprite from "./Sprite.js";
import * as Backend from "./Backend.js";
import * as Particle from "./Particle.js";
import * as Viewport from "./Viewport.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as AuthClient from "./AuthClient.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

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
    var r2 = $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2, state.level);
    o1.vy = -Config.dampenJump;
    o1.py = o1.py - 5;
    return [
            undefined,
            r2
          ];
  }
  $$Object.decHealth(o2);
  o1.vy = -Config.dampenJump;
  if (state.multiplier === 8) {
    State.updateScore(state, 800);
    o2.score = 800;
    return [
            undefined,
            $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2, state.level)
          ];
  }
  var score = Math.imul(100, state.multiplier);
  State.updateScore(state, score);
  o2.score = score;
  state.multiplier = (state.multiplier << 1);
  return [
          undefined,
          $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2, state.level)
        ];
}

function enemyAttackPlayer(enemy, player, level) {
  var enemyTyp = enemy.objTyp;
  if (enemyTyp.TAG === /* Enemy */2) {
    var enemyTyp$1 = enemyTyp._0;
    if (enemyTyp$1 >= 3 && enemy.vx === 0) {
      var r2 = $$Object.evolveEnemy(player.dir, enemyTyp$1, enemy.sprite, enemy, level);
      return [
              undefined,
              r2
            ];
    }
    
  }
  $$Object.decHealth(player);
  player.invuln = Config.invuln;
  return [
          undefined,
          undefined
        ];
}

function collEnemyEnemy(enemy1, s1, o1, enemy2, s2, o2, dir2) {
  if (enemy1 !== 3) {
    if (enemy1 < 4) {
      if (enemy2 >= 3) {
        if (o2.vx === 0) {
          $$Object.revDir(o1, enemy1, s1);
          return [
                  undefined,
                  undefined
                ];
        } else {
          $$Object.decHealth(o1);
          return [
                  undefined,
                  undefined
                ];
        }
      } else if (dir2 >= 2) {
        $$Object.revDir(o1, enemy1, s1);
        $$Object.revDir(o2, enemy2, s2);
        return [
                undefined,
                undefined
              ];
      } else {
        return [
                undefined,
                undefined
              ];
      }
    }
    if (enemy2 >= 3) {
      $$Object.decHealth(o1);
      $$Object.decHealth(o2);
      return [
              undefined,
              undefined
            ];
    }
    
  } else if (enemy2 >= 3) {
    $$Object.decHealth(o1);
    $$Object.decHealth(o2);
    return [
            undefined,
            undefined
          ];
  }
  if (o1.vx === 0) {
    $$Object.revDir(o2, enemy2, s2);
    return [
            undefined,
            undefined
          ];
  } else {
    $$Object.decHealth(o2);
    return [
            undefined,
            undefined
          ];
  }
}

var $$global = {
  state: State.$$new(3, 0),
  status: /* Playing */2
};

function reset(level, score) {
  $$global.state = State.$$new(level, score);
  $$global.status = /* Playing */2;
  
}

var Global = {
  $$global: $$global,
  reset: reset
};

function loadState(principal) {
  return Backend.actor.loadGameState(principal).then(function (json) {
              if (json !== "") {
                $$global.state = JSON.parse(json);
              }
              return Promise.resolve(undefined);
            });
}

function saveState(principal) {
  return Backend.actor.saveGameState(principal, JSON.stringify($$global.state));
}

function loadStateBinary(principal) {
  return Backend.actor.loadGameStateNative(principal).then(function (arr) {
              if (arr.length === 1) {
                var state = arr[0];
                $$global.state = state;
              }
              return Promise.resolve(undefined);
            });
}

function saveStateBinary(principal) {
  return Backend.actor.saveGameStateNative(principal, $$global.state);
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
              return [
                      undefined,
                      undefined
                    ];
          case /* Block */4 :
              var t2$2 = t2$1._0;
              if (dir2 >= 2) {
                var exit$3 = 0;
                if (t1$1 !== 3) {
                  if (t1$1 >= 4) {
                    if (t2$2 !== 3) {
                      exit$3 = 5;
                    } else {
                      $$Object.decHealth(collid);
                      $$Object.reverseLeftRight(obj);
                      return [
                              undefined,
                              undefined
                            ];
                    }
                  } else {
                    $$Object.revDir(obj, t1$1, s1);
                    return [
                            undefined,
                            undefined
                          ];
                  }
                } else if (t2$2 !== 3) {
                  exit$3 = 5;
                } else {
                  $$Object.decHealth(collid);
                  $$Object.reverseLeftRight(obj);
                  return [
                          undefined,
                          undefined
                        ];
                }
                if (exit$3 === 5) {
                  if (t2$2 !== 1) {
                    if (t2$2 !== 0) {
                      $$Object.revDir(obj, t1$1, s1);
                      return [
                              undefined,
                              undefined
                            ];
                    }
                    var updatedBlock = $$Object.evolveBlock(collid, state.level);
                    var spawnedItem = $$Object.spawnAbove(obj.dir, collid, /* Mushroom */0, state.level);
                    $$Object.revDir(obj, t1$1, s1);
                    return [
                            updatedBlock,
                            spawnedItem
                          ];
                  }
                  var updatedBlock$1 = $$Object.evolveBlock(collid, state.level);
                  var spawnedItem$1 = $$Object.spawnAbove(obj.dir, collid, /* Coin */1, state.level);
                  $$Object.revDir(obj, t1$1, s1);
                  return [
                          updatedBlock$1,
                          spawnedItem$1
                        ];
                }
                
              } else {
                $$Object.collideBlock(dir2, obj);
                return [
                        undefined,
                        undefined
                      ];
              }
              break;
          
        }
        if (exit$2 === 4) {
          if (dir2 !== 0) {
            return enemyAttackPlayer(obj, collid, state.level);
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
              return [
                      undefined,
                      undefined
                    ];
          case /* Block */4 :
              if (dir2 >= 2) {
                $$Object.reverseLeftRight(obj);
                return [
                        undefined,
                        undefined
                      ];
              } else {
                $$Object.collideBlock(dir2, obj);
                return [
                        undefined,
                        undefined
                      ];
              }
          
        }
        break;
    case /* Block */4 :
        return [
                undefined,
                undefined
              ];
    
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
            return enemyAttackPlayer(collid, obj, state.level);
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
              return [
                      undefined,
                      undefined
                    ];
            }
            if (dir2 !== 0) {
              var exit$6 = 0;
              exit$6 = 6;
              if (exit$6 === 6) {
                var t$1 = collid.objTyp;
                if (t$1.TAG === /* Block */4) {
                  if (t$1._0 !== 6) {
                    if (dir2 !== 1) {
                      $$Object.collideBlock(dir2, obj);
                      return [
                              undefined,
                              undefined
                            ];
                    } else {
                      state.multiplier = 1;
                      $$Object.collideBlock(dir2, obj);
                      return [
                              undefined,
                              undefined
                            ];
                    }
                  } else {
                    $$global.status = {
                      TAG: /* Finished */1,
                      levelResult: /* Won */0,
                      restartTime: Config.delayWhenFinished + performance.now()
                    };
                    return [
                            undefined,
                            undefined
                          ];
                  }
                } else {
                  return [
                          undefined,
                          undefined
                        ];
                }
              }
              
            } else {
              var exit$7 = 0;
              switch (t._0) {
                case /* QBlockMushroom */0 :
                    var updatedBlock$2 = $$Object.evolveBlock(collid, state.level);
                    var spawnedItem$2 = $$Object.spawnAbove(obj.dir, collid, /* Mushroom */0, state.level);
                    $$Object.collideBlock(dir2, obj);
                    return [
                            spawnedItem$2,
                            updatedBlock$2
                          ];
                case /* QBlockCoin */1 :
                    var updatedBlock$3 = $$Object.evolveBlock(collid, state.level);
                    var spawnedItem$3 = $$Object.spawnAbove(obj.dir, collid, /* Coin */1, state.level);
                    $$Object.collideBlock(dir2, obj);
                    return [
                            spawnedItem$3,
                            updatedBlock$3
                          ];
                case /* Brick */3 :
                    if (t1$2 === /* BigM */0) {
                      $$Object.collideBlock(dir2, obj);
                      $$Object.decHealth(collid);
                      return [
                              undefined,
                              undefined
                            ];
                    } else {
                      $$Object.collideBlock(dir2, obj);
                      return [
                              undefined,
                              undefined
                            ];
                    }
                case /* Panel */6 :
                    $$global.status = {
                      TAG: /* Finished */1,
                      levelResult: /* Won */0,
                      restartTime: Config.delayWhenFinished + performance.now()
                    };
                    return [
                            undefined,
                            undefined
                          ];
                case /* QBlockUsed */2 :
                case /* UnBBlock */4 :
                case /* Cloud */5 :
                case /* Ground */7 :
                    exit$7 = 6;
                    break;
                
              }
              if (exit$7 === 6) {
                $$Object.collideBlock(dir2, obj);
                return [
                        undefined,
                        undefined
                      ];
              }
              
            }
          }
          break;
      
    }
    if (exit$4 === 4) {
      if (dir2 >= 2) {
        collid.vx = collid.vx + obj.vx;
        return [
                undefined,
                undefined
              ];
      } else {
        return [
                undefined,
                undefined
              ];
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
          State.updateScore(state, 100);
          return [
                  undefined,
                  undefined
                ];
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
          return [
                  undefined,
                  undefined
                ];
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

function broadPhase(allCollids, viewport) {
  return Belt_List.keep(allCollids, (function (o) {
                return inViewport(o, viewport);
              }));
}

function narrowPhase(obj, state, visibleCollids) {
  var _visibleCollids = visibleCollids;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var visibleCollids$1 = _visibleCollids;
    if (!visibleCollids$1) {
      return acc;
    }
    var collid = visibleCollids$1.hd;
    var newObjs;
    if ($$Object.sameId(obj, collid)) {
      newObjs = [
        undefined,
        undefined
      ];
    } else {
      var dir = $$Object.checkCollision(obj, collid);
      newObjs = dir !== undefined ? processCollision(dir, obj, collid, state) : [
          undefined,
          undefined
        ];
    }
    var o = newObjs[0];
    var acc$1;
    if (o !== undefined) {
      var o2 = newObjs[1];
      acc$1 = o2 !== undefined ? ({
            hd: o,
            tl: {
              hd: o2,
              tl: acc
            }
          }) : ({
            hd: o,
            tl: acc
          });
    } else {
      var o$1 = newObjs[1];
      acc$1 = o$1 !== undefined ? ({
            hd: o$1,
            tl: acc
          }) : acc;
    }
    _acc = acc$1;
    _visibleCollids = visibleCollids$1.tl;
    continue ;
  };
}

function checkCollisions(obj, allCollids, players, state) {
  var match = obj.objTyp;
  if (match.TAG === /* Block */4) {
    return /* [] */0;
  }
  var visibleCollids = Belt_List.concat(players, broadPhase(allCollids, state.viewport));
  return narrowPhase(obj, state, visibleCollids);
}

function findObjectsColliding(obj, allCollids, players, state) {
  var sprite = obj.sprite;
  obj.invuln = obj.invuln > 0 ? obj.invuln - 1 | 0 : 0;
  if (!((!obj.kill || $$Object.isPlayer(obj)) && inViewport(obj, state.viewport))) {
    return /* [] */0;
  }
  obj.grounded = false;
  $$Object.processObj(obj, state.level);
  var objectsColliding = checkCollisions(obj, allCollids, players, state);
  if (obj.vx !== 0 || !$$Object.isEnemy(obj)) {
    Sprite.updateAnimation(sprite);
  }
  return objectsColliding;
}

function updateObject(obj, allCollids, players, state) {
  var match = obj.objTyp;
  switch (match.TAG | 0) {
    case /* Player1 */0 :
    case /* Player2 */1 :
        break;
    default:
      var objectsColliding = findObjectsColliding(obj, allCollids, players, state);
      if (!obj.kill) {
        state.objects = {
          hd: obj,
          tl: Pervasives.$at(objectsColliding, state.objects)
        };
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
  var objectsColliding$1 = findObjectsColliding(obj, allCollids, players, state);
  state.objects = Pervasives.$at(objectsColliding$1, state.objects);
  
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
      $$global.status = {
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
              $$global.status = /* Playing */2;
              
            }), 30);
      
    };
    var match = Keys.pressedKeys.pendingStateOperations;
    if (match !== undefined) {
      Keys.pressedKeys.pendingStateOperations = undefined;
      if (match) {
        var doSave = function (principal) {
          console.log("saving...");
          $$global.status = /* Saving */3;
          saveStateBinary(principal).then(function (param) {
                console.log("saved");
                $$global.status = /* Playing */2;
                
              });
          
        };
        var principal = auth.contents;
        if (principal) {
          doSave(principal._0);
        } else {
          startLogin(doSave, /* Save */1);
        }
      } else {
        var doLoad = function (principal) {
          console.log("loading...");
          $$global.status = /* Loading */0;
          loadStateBinary(principal).then(function (param) {
                console.log("loaded");
                $$global.status = /* Playing */2;
                
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
      $$global.status = /* Paused */1;
    } else if ($$global.status === /* Paused */1) {
      $$global.status = /* Playing */2;
    }
    var loadOrSave = $$global.status;
    if (typeof loadOrSave === "number") {
      switch (loadOrSave) {
        case /* Loading */0 :
            Draw.drawState($$global.state, 0);
            Draw.loading(undefined);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        case /* Paused */1 :
            Draw.drawState($$global.state, 0);
            Draw.paused(undefined);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        case /* Playing */2 :
            var fps = calcFps(undefined);
            var oldObjects = $$global.state.objects;
            $$global.state.objects = /* [] */0;
            $$global.state.particles = Belt_Array.keep($$global.state.particles, updateParticle);
            var players = Keys.checkTwoPlayers(undefined) ? ({
                  hd: $$global.state.player1,
                  tl: {
                    hd: $$global.state.player2,
                    tl: /* [] */0
                  }
                }) : ({
                  hd: $$global.state.player1,
                  tl: /* [] */0
                });
            updateObject($$global.state.player1, oldObjects, players, $$global.state);
            if (Keys.checkTwoPlayers(undefined)) {
              updateObject($$global.state.player2, oldObjects, players, $$global.state);
            }
            if ($$global.state.player1.kill) {
              $$global.status = {
                TAG: /* Finished */1,
                levelResult: /* Lost */1,
                restartTime: Config.delayWhenFinished + performance.now()
              };
            }
            Viewport.update($$global.state.viewport, $$global.state.player1.px, $$global.state.player1.py);
            Belt_List.forEach(oldObjects, (function(oldObjects,players){
                return function (obj) {
                  return updateObject(obj, oldObjects, players, $$global.state);
                }
                }(oldObjects,players)));
            Draw.drawState($$global.state, fps);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        case /* Saving */3 :
            Draw.drawState($$global.state, 0);
            Draw.saving(undefined);
            requestAnimationFrame(function (param) {
                  return updateLoop(undefined);
                });
            return ;
        
      }
    } else {
      if (loadOrSave.TAG === /* LoggingIn */0) {
        Draw.drawState($$global.state, 0);
        Draw.loggingIn(loadOrSave._0);
        requestAnimationFrame(function (param) {
              return updateLoop(undefined);
            });
        return ;
      }
      var levelResult = loadOrSave.levelResult;
      var timeToStart = (loadOrSave.restartTime - performance.now()) / 1000;
      if (timeToStart > 0.9) {
        Draw.levelFinished(levelResult, String($$global.state.level), String(timeToStart | 0));
        requestAnimationFrame(function (param) {
              return updateLoop(undefined);
            });
        return ;
      }
      var level = levelResult === /* Won */0 ? $$global.state.level + 1 | 0 : $$global.state.level;
      var score = levelResult === /* Won */0 ? $$global.state.score : 0;
      reset(level, score);
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
  $$global ,
  loadState ,
  saveState ,
  loadStateBinary ,
  saveStateBinary ,
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
