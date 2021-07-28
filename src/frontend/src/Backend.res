module Candid = {
  type principal

  module Actor = {
    type t = {
      loadDelta: (. principal) => Js.Promise.t<array<Types.delta>>,
      saveDelta: (. principal, Types.delta) => Js.Promise.t<unit>,
    }
  }
}

@module("dfx-generated/backend")
external backend: Candid.Actor.t = "backend"

let actor = backend