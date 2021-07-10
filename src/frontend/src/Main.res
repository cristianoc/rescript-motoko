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

@react.component
let main = () => {
  let (state, updateState) = React.useState(() => "hello")
  let onNewTree = newTree => {
    let newState = newTree->printTree
    if newState != state {
      updateState(_ => newState)
    }
  }
  let onTick = () => {
    service.get()->Promise.thenResolve(onNewTree)->ignore
  }
  React.useEffect0(() => {
    let intervalId = Js.Global.setInterval(onTick, 1000)
    Some(() => intervalId->Js.Global.clearInterval)
  })
  let button = (name, cb) =>
    <button
      className="clickMeBtn"
      onClick={_ => {
        cb()->ignore
      }}>
      {React.string(name)}
    </button>
  <div className="top">
    <div className="container"> {React.string(state)} </div>
    <section>
      {button("Extend", service.extend)}
      {button("Reverse", service.reverseInPlace)}
      {button("Reset", service.reset)}
    </section>
  </div>
}
