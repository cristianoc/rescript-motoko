module Candid = {
  type principal

  module Actor = {
    type t = {
      loadGameState: (. principal) => Js.Promise.t<string>,
      saveGameState: (. principal, string) => Js.Promise.t<unit>,
      loadGameStateNative: (. principal) => Js.Promise.t<array<Types.state>>,
      saveGameStateNative: (. principal, Types.state) => Js.Promise.t<unit>,
    }
  }
}

@module("dfx-generated/backend")
external canisterId: Agent.canisterId = "canisterId"

@module("dfx-generated/backend")
external idlFactory: Agent.idlFactory<Candid.Actor.t> = "idlFactory"

let actor = Agent.createActor(~idlFactory, ~canisterId)
