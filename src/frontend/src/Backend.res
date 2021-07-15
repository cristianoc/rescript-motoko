@module("dfx-generated/backend")
external canisterId: Agent.canisterId = "canisterId"

@module("dfx-generated/backend")
external idlFactory: Agent.idlFactory<Candid.Actor.t> = "idlFactory"

let actor = Agent.createActor(~idlFactory, ~canisterId)
