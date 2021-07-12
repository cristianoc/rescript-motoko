// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Agent from "./Agent.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Candid from "./Candid.js";
import * as Backend from "dfx-generated/backend";

var actor = Agent.createActor(Backend.idlFactory, Backend.canisterId);

var service = Candid.Service.fromActor(actor);

function printTree(t) {
  if (t.TAG === /* Leaf */0) {
    return String(t._0);
  } else {
    return "(" + printTree(t._0) + ", " + printTree(t._1) + ")";
  }
}

var current = {
  contents: "hello"
};

function onTick(param) {
  Curry._1(service.get, undefined).then(function (t) {
        current.contents = printTree(t);
        
      });
  
}

export {
  actor ,
  service ,
  printTree ,
  current ,
  onTick ,
  
}
/* actor Not a pure module */
