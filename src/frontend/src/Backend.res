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
external canisterId: Agent.canisterId = "canisterId"

@module("dfx-generated/backend")
external idlFactory: Agent.idlFactory<Candid.Actor.t> = "idlFactory"

let actor = Agent.createActor(~idlFactory, ~canisterId)
