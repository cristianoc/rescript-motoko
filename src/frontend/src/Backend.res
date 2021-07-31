module Candid = {
  type principal

  type highScores = array<{"name": string, "score": int}>

  module Actor = {
    type t = {
      highScores: (. unit) => Js.Promise.t<highScores>,
      loadDelta: (. principal) => Js.Promise.t<array<Types.delta>>,
      saveDelta: (. principal, Types.delta) => Js.Promise.t<unit>,
    }
  }
}

@module("dfx-generated/backend")
external backend: Candid.Actor.t = "backend"

let actor = backend
