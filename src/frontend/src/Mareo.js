// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Backend from "./Backend.js";
import * as Director from "./Director.js";

function start(param) {
  window.onload = (function (param) {
      Director.updateLoop(undefined);
      setInterval(Backend.onTick, 1000);
      return true;
    });
  
}

export {
  start ,
  
}
/* Backend Not a pure module */
