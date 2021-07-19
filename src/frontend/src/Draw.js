// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Keys from "./Keys.js";
import * as Load from "./Load.js";
import * as Config from "./Config.js";
import * as Sprite from "./Sprite.js";
import * as Viewport from "./Viewport.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";

function renderBbox(sprite, posx, posy) {
  var match = sprite.params.bboxOffset;
  var match$1 = sprite.params.bboxSize;
  var context = Load.getContext(undefined);
  context.strokeStyle = "#FF0000";
  return context.strokeRect(posx + match[0], posy + match[1], match$1[0], match$1[1]);
}

function render(sprite, posx, posy) {
  var match = sprite.params.srcOffset;
  var match$1 = sprite.params.frameSize;
  var sw = match$1[0];
  var match$2 = sprite.params.frameSize;
  var sx = match[0] + sprite.frame * sw;
  var context = Load.getContext(undefined);
  return context.drawImage(Sprite.Png.toImg(sprite.params.png), sx, match[1], sw, match$1[1], posx, posy, match$2[0], match$2[1]);
}

function drawBgd(state) {
  var vposXInt = state.viewport.px / 5 | 0;
  var bgdWidth = state.bgd.params.frameSize[0] | 0;
  var off_x = Caml_int32.mod_(vposXInt, bgdWidth);
  var bgd = state.bgd;
  render(bgd, -off_x, 0);
  return render(bgd, bgd.params.frameSize[0] - off_x, 0);
}

function clearCanvas(param) {
  var match = Load.getCanvasData(undefined);
  var match$1 = match.sizeScaled;
  return Load.getContext(undefined).clearRect(0, 0, match$1.widthScaled, match$1.heightScaled);
}

var fontPx = String(Config.fontSize | 0) + "px";

function centerXText(txt, y) {
  var ctx = Load.getContext(undefined);
  var match = Load.getCanvasData(undefined);
  ctx.font = fontPx + "'Press Start 2P'";
  var xCentered = (match.sizeScaled.widthScaled - Config.fontSize * txt.length) / 2;
  return ctx.fillText(txt, xCentered, y);
}

function centerXYText(txt) {
  var match = Load.getCanvasData(undefined);
  var yCentered = 0.5 * match.sizeScaled.heightScaled;
  return centerXText(txt, yCentered);
}

function scoreAndCoins(score, coins) {
  var coin_string = String(coins);
  var context = Load.getContext(undefined);
  context.font = fontPx + " 'Press Start 2P'";
  context.fillText("Cx" + coin_string, Config.fontSize, Config.fontSize * 2);
  var match = Load.getCanvasData(undefined);
  var scoreTxt = String(score);
  return context.fillText(scoreTxt, match.sizeScaled.widthScaled - (scoreTxt.length + 1 | 0) * Config.fontSize, Config.fontSize * 2);
}

function fps(fps_val) {
  var fps_str = String(fps_val | 0);
  return centerXText(fps_str, Config.fontSize * 2);
}

function loggingIn(loadOrSave) {
  return centerXYText("Logging in before " + (
              loadOrSave ? "saving" : "loading"
            ));
}

function loading(param) {
  return centerXYText("Loading...");
}

function saving(param) {
  return centerXYText("Saving...");
}

function paused(param) {
  return centerXYText("Paused");
}

function blackScreen(texts) {
  var ctx = Load.getContext(undefined);
  var match = Load.getCanvasData(undefined);
  var match$1 = match.sizeScaled;
  var heightScaled = match$1.heightScaled;
  ctx.rect(0, 0, match$1.widthScaled, heightScaled);
  ctx.fillStyle = "black";
  ctx.fill();
  ctx.fillStyle = "white";
  Belt_List.forEach(texts, (function (param) {
          return centerXText(param[0], param[1] * heightScaled);
        }));
  ctx.fillStyle = "black";
  
}

function levelFinished(result, level, elapsed) {
  if (result) {
    return blackScreen({
                hd: [
                  "You lose level " + (level + "!"),
                  0.4
                ],
                tl: {
                  hd: [
                    elapsed,
                    0.6
                  ],
                  tl: /* [] */0
                }
              });
  } else {
    return blackScreen({
                hd: [
                  "You win level" + (level + "!"),
                  0.4
                ],
                tl: {
                  hd: [
                    elapsed,
                    0.6
                  ],
                  tl: /* [] */0
                }
              });
  }
}

function particles(particles$1, viewport) {
  return Belt_List.forEach(particles$1, (function (part) {
                var x = part.px - viewport.px;
                var y = part.py - viewport.py;
                return render(part.params.sprite, x, y);
              }));
}

function object(obj, viewport) {
  var match = Viewport.fromCoord(viewport, obj.px, obj.py);
  var x = match.x;
  var y = match.y;
  render(obj.sprite, x, y);
  if (Keys.checkBboxEnabled(undefined)) {
    return renderBbox(obj.sprite, x, y);
  }
  
}

function drawState(state, fps_) {
  var objectsWihtPlayer1_0 = state.player1;
  var objectsWihtPlayer1_1 = state.objects;
  var objectsWihtPlayer1 = {
    hd: objectsWihtPlayer1_0,
    tl: objectsWihtPlayer1_1
  };
  var objectsWithPlayers = Keys.checkTwoPlayers(undefined) ? ({
        hd: state.player2,
        tl: objectsWihtPlayer1
      }) : objectsWihtPlayer1;
  clearCanvas(undefined);
  drawBgd(state);
  Belt_List.forEach(objectsWithPlayers, (function (obj) {
          return object(obj, state.viewport);
        }));
  particles(state.particles, state.viewport);
  fps(fps_);
  return scoreAndCoins(state.score, state.coins);
}

export {
  renderBbox ,
  render ,
  drawBgd ,
  clearCanvas ,
  fontPx ,
  centerXText ,
  centerXYText ,
  scoreAndCoins ,
  fps ,
  loggingIn ,
  loading ,
  saving ,
  paused ,
  blackScreen ,
  levelFinished ,
  particles ,
  object ,
  drawState ,
  
}
/* fontPx Not a pure module */
