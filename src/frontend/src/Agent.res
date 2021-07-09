type t // agent
type idlFactory<'actor>
type canisterId

@module("@dfinity/agent") @new
external httpAgent: unit => t = "HttpAgent"

@module("@dfinity/agent")
external _Actor: {
  "createActor": (. idlFactory<'actor>, {"agent": t, "canisterId": canisterId}) => 'actor,
} = "Actor"

let createActor = (~idlFactory, ~canisterId) => {
  let agent = httpAgent()
  _Actor["createActor"](. idlFactory, {"agent": agent, "canisterId": canisterId})
}
