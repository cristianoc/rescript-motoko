// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Draw from "./Draw.js";
import * as Keys from "./Keys.js";
import * as State from "./State.js";
import * as Config from "./Config.js";
import * as $$Object from "./Object.js";
import * as Sprite from "./Sprite.js";
import * as Particle from "./Particle.js";
import * as Viewport from "./Viewport.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
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
    var r2 = $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2);
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
            $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2)
          ];
  }
  var score = Math.imul(100, state.multiplier);
  State.updateScore(state, score);
  o2.score = score;
  state.multiplier = (state.multiplier << 1);
  return [
          undefined,
          $$Object.evolveEnemy(o1.dir, enemyTyp, s2, o2)
        ];
}

function enemyAttackPlayer(enemy, player) {
  var enemyTyp = enemy.objTyp;
  if (enemyTyp.TAG === /* Enemy */1) {
    var enemyTyp$1 = enemyTyp._0;
    if (enemyTyp$1 >= 3 && enemy.vx === 0) {
      var r2 = $$Object.evolveEnemy(player.dir, enemyTyp$1, enemy.sprite, enemy);
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

function collEnemyEnemy(enemy1, s1, o1, enemy2, s2, o2, dir) {
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
      } else if (dir >= 2) {
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

function processCollision(dir, obj, collid, state) {
  var t2;
  var t1 = obj.objTyp;
  switch (t1.TAG | 0) {
    case /* Player */0 :
        var t = collid.objTyp;
        switch (t.TAG | 0) {
          case /* Player */0 :
              if (dir >= 2) {
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
          case /* Enemy */1 :
              var s2 = collid.sprite;
              if (dir !== 1) {
                return enemyAttackPlayer(collid, obj);
              } else {
                return playerAttackEnemy(obj, t._0, s2, collid, state);
              }
          case /* Item */2 :
              t2 = t._0;
              break;
          case /* Block */3 :
              var t$1 = t._0;
              if (dir !== 0) {
                if (t$1 === 4) {
                  state.status = /* Finished */{
                    levelResult: /* Won */0,
                    restartTime: Config.delayWhenFinished + performance.now()
                  };
                  return [
                          undefined,
                          undefined
                        ];
                } else if (dir !== 1) {
                  $$Object.collideBlock(dir, obj);
                  return [
                          undefined,
                          undefined
                        ];
                } else {
                  state.multiplier = 1;
                  $$Object.collideBlock(dir, obj);
                  return [
                          undefined,
                          undefined
                        ];
                }
              }
              if (typeof t$1 === "number") {
                if (t$1 !== 1) {
                  if (t$1 !== 4) {
                    $$Object.collideBlock(dir, obj);
                    return [
                            undefined,
                            undefined
                          ];
                  } else {
                    state.status = /* Finished */{
                      levelResult: /* Won */0,
                      restartTime: Config.delayWhenFinished + performance.now()
                    };
                    return [
                            undefined,
                            undefined
                          ];
                  }
                } else if (t1._0 === /* BigM */0) {
                  $$Object.collideBlock(dir, obj);
                  $$Object.decHealth(collid);
                  return [
                          undefined,
                          undefined
                        ];
                } else {
                  $$Object.collideBlock(dir, obj);
                  return [
                          undefined,
                          undefined
                        ];
                }
              }
              var updatedBlock = $$Object.evolveBlock(collid);
              var spawnedItem = $$Object.spawnAbove(obj.dir, collid, t$1._0);
              $$Object.collideBlock(dir, obj);
              return [
                      spawnedItem,
                      updatedBlock
                    ];
          
        }
        break;
    case /* Enemy */1 :
        var t1$1 = t1._0;
        var s1 = obj.sprite;
        var t2$1 = collid.objTyp;
        switch (t2$1.TAG | 0) {
          case /* Player */0 :
              if (dir !== 0) {
                return enemyAttackPlayer(obj, collid);
              } else {
                return playerAttackEnemy(obj, t1$1, s1, collid, state);
              }
          case /* Enemy */1 :
              var s2$1 = collid.sprite;
              return collEnemyEnemy(t1$1, s1, obj, t2$1._0, s2$1, collid, dir);
          case /* Item */2 :
              return [
                      undefined,
                      undefined
                    ];
          case /* Block */3 :
              var t2$2 = t2$1._0;
              if (dir >= 2) {
                if (t1$1 >= 3) {
                  if (typeof t2$2 === "number") {
                    if (t2$2 !== 1) {
                      $$Object.revDir(obj, t1$1, s1);
                      return [
                              undefined,
                              undefined
                            ];
                    } else {
                      $$Object.decHealth(collid);
                      $$Object.reverseLeftRight(obj);
                      return [
                              undefined,
                              undefined
                            ];
                    }
                  }
                  var updatedBlock$1 = $$Object.evolveBlock(collid);
                  var spawnedItem$1 = $$Object.spawnAbove(obj.dir, collid, t2$2._0);
                  $$Object.revDir(obj, t1$1, s1);
                  return [
                          updatedBlock$1,
                          spawnedItem$1
                        ];
                }
                $$Object.revDir(obj, t1$1, s1);
                return [
                        undefined,
                        undefined
                      ];
              }
              $$Object.collideBlock(dir, obj);
              return [
                      undefined,
                      undefined
                    ];
          
        }
    case /* Item */2 :
        var match = collid.objTyp;
        switch (match.TAG | 0) {
          case /* Player */0 :
              t2 = t1._0;
              break;
          case /* Enemy */1 :
          case /* Item */2 :
              return [
                      undefined,
                      undefined
                    ];
          case /* Block */3 :
              if (dir >= 2) {
                $$Object.reverseLeftRight(obj);
                return [
                        undefined,
                        undefined
                      ];
              } else {
                $$Object.collideBlock(dir, obj);
                return [
                        undefined,
                        undefined
                      ];
              }
          
        }
        break;
    case /* Block */3 :
        return [
                undefined,
                undefined
              ];
    
  }
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

function checkCollisions(obj, state, allCollids) {
  var match = obj.objTyp;
  if (match.TAG === /* Block */3) {
    return /* [] */0;
  }
  var visibleCollids = broadPhase(allCollids, state.viewport);
  return narrowPhase(obj, state, visibleCollids);
}

function findObjectsColliding(allCollids, obj, state) {
  var sprite = obj.sprite;
  obj.invuln = obj.invuln > 0 ? obj.invuln - 1 | 0 : 0;
  if (!((!obj.kill || $$Object.isPlayer(obj)) && inViewport(obj, state.viewport))) {
    return /* [] */0;
  }
  obj.grounded = false;
  $$Object.processObj(obj, state.level);
  var objectsColliding = checkCollisions(obj, state, allCollids);
  if (obj.vx !== 0 || !$$Object.isEnemy(obj)) {
    Sprite.updateAnimation(sprite);
  }
  return objectsColliding;
}

function updateObject(allCollids, obj, state) {
  var match = obj.objTyp;
  if (match.TAG === /* Player */0) {
    var playerNum = match._1;
    var keys = Keys.translateKeys(playerNum);
    obj.crouch = false;
    $$Object.updatePlayer(obj, playerNum, keys);
    var objectsColliding = findObjectsColliding(allCollids, obj, state);
    state.objects = Pervasives.$at(objectsColliding, state.objects);
    return ;
  }
  var objectsColliding$1 = findObjectsColliding(allCollids, obj, state);
  if (!obj.kill) {
    state.objects = {
      hd: obj,
      tl: Pervasives.$at(objectsColliding$1, state.objects)
    };
  }
  var newParts = obj.kill ? $$Object.kill(obj) : /* [] */0;
  state.particles = Pervasives.$at(newParts, state.particles);
  
}

function updateParticle(part) {
  Particle.$$process(part);
  return !part.kill;
}

function updateLoop(_param) {
  while(true) {
    var match = State.current.contents.status;
    if (Keys.checkPaused(undefined)) {
      Draw.drawState(State.current.contents, 0);
      Draw.paused(undefined);
      requestAnimationFrame(function (param) {
            return updateLoop(undefined);
          });
      return ;
    }
    if (match) {
      var levelResult = match.levelResult;
      var timeToStart = (match.restartTime - performance.now()) / 1000;
      if (timeToStart > 0.9) {
        Draw.levelFinished(levelResult, String(State.current.contents.level), String(timeToStart | 0));
        requestAnimationFrame(function (param) {
              return updateLoop(undefined);
            });
        return ;
      }
      var level = levelResult === /* Won */0 ? State.current.contents.level + 1 | 0 : State.current.contents.level;
      State.current.contents = State.$$new(level);
      _param = undefined;
      continue ;
    }
    var fps = calcFps(undefined);
    var oldObjects = State.current.contents.objects;
    State.current.contents.objects = /* [] */0;
    State.current.contents.particles = Belt_List.keep(State.current.contents.particles, updateParticle);
    updateObject(Keys.checkTwoPlayers(undefined) ? ({
              hd: State.current.contents.player2,
              tl: oldObjects
            }) : oldObjects, State.current.contents.player1, State.current.contents);
    if (Keys.checkTwoPlayers(undefined)) {
      updateObject({
            hd: State.current.contents.player1,
            tl: oldObjects
          }, State.current.contents.player2, State.current.contents);
    }
    if (State.current.contents.player1.kill) {
      State.current.contents.status = /* Finished */{
        levelResult: /* Lost */1,
        restartTime: Config.delayWhenFinished + performance.now()
      };
    }
    Viewport.update(State.current.contents.viewport, State.current.contents.player1.px, State.current.contents.player1.py);
    Belt_List.forEach(oldObjects, (function(oldObjects){
        return function (obj) {
          return updateObject(oldObjects, obj, State.current.contents);
        }
        }(oldObjects)));
    Draw.drawState(State.current.contents, fps);
    requestAnimationFrame(function (param) {
          return updateLoop(undefined);
        });
    return ;
  };
}

export {
  calcFps ,
  playerAttackEnemy ,
  enemyAttackPlayer ,
  collEnemyEnemy ,
  processCollision ,
  inViewport ,
  broadPhase ,
  narrowPhase ,
  checkCollisions ,
  findObjectsColliding ,
  updateObject ,
  updateParticle ,
  updateLoop ,
  
}
/* Draw Not a pure module */
