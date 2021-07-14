// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Sprite from "./Sprite.js";
import * as Backend from "./Backend.js";
import * as Viewport from "./Viewport.js";
import * as Generator from "./Generator.js";

function $$new(level) {
  var player1 = Generator.newPlayer(/* One */0);
  var player2 = Generator.newPlayer(/* Two */1);
  var viewport = Viewport.make(level);
  Viewport.update(viewport, player1.px, player1.py);
  var objects = Generator.generate(level);
  return {
          bgd: Sprite.makeBgd(undefined),
          coins: 0,
          level: level,
          multiplier: 1,
          objects: objects,
          particles: /* [] */0,
          player1: player1,
          player2: player2,
          score: 0,
          status: /* Playing */2,
          viewport: viewport
        };
}

function updateScore(state, i) {
  state.score = state.score + i | 0;
  
}

var current = {
  contents: $$new(1)
};

function load(param) {
  return Curry._1(Backend.service.loadGameState, undefined).then(function (json) {
              current.contents = JSON.parse(json);
              return Promise.resolve(undefined);
            });
}

function save(param) {
  return Curry._1(Backend.service.saveGameState, JSON.stringify(current.contents));
}

export {
  $$new ,
  updateScore ,
  current ,
  load ,
  save ,
  
}
/* current Not a pure module */
