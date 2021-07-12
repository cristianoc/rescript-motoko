// Generated by ReScript, PLEASE EDIT WITH CARE


function fromCandid(raw) {
  if (raw.Leaf !== undefined) {
    return {
            TAG: /* Leaf */0,
            _0: raw.Leaf
          };
  }
  var match = raw.Node;
  return {
          TAG: /* Node */1,
          _0: fromCandid(match[0]),
          _1: fromCandid(match[1])
        };
}

function toCandid(t) {
  if (t.TAG === /* Leaf */0) {
    return {
            Leaf: t._0
          };
  } else {
    return {
            Node: [
              toCandid(t._0),
              toCandid(t._1)
            ]
          };
  }
}

var Tree = {
  fromCandid: fromCandid,
  toCandid: toCandid
};

var Actor = {};

function fromActor(actor) {
  return {
          extend: (function (param) {
              return actor.extend();
            }),
          loadGameState: (function (param) {
              return actor.loadGameState();
            }),
          get: (function (param) {
              return actor.get().then(fromCandid);
            }),
          set: (function (t) {
              return actor.set(toCandid(t));
            }),
          reverse: (function (param) {
              return actor.reverse();
            }),
          reset: (function (param) {
              return actor.reset();
            }),
          saveGameState: (function (s) {
              return actor.saveGameState(s);
            })
        };
}

var Service = {
  fromActor: fromActor
};

export {
  Tree ,
  Actor ,
  Service ,
  
}
/* No side effect */
