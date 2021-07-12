@module("dfx-generated/backend")
external canisterId: Agent.canisterId = "canisterId"

@module("dfx-generated/backend")
external idlFactory: Agent.idlFactory<Candid.Actor.t> = "idlFactory"

let actor = Agent.createActor(~idlFactory, ~canisterId)

let service = actor->Candid.Service.fromActor

let rec printTree = (t: Candid.Tree.t) => {
  switch t {
  | Leaf(n) => string_of_int(n)
  | Node(t1, t2) => "(" ++ printTree(t1) ++ ", " ++ printTree(t2) ++ ")"
  }
}

let current = ref("hello")
let onTick = () => {
  service.get()->Promise.thenResolve(t => current := t->printTree)->ignore
}
