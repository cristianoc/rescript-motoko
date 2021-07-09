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
  let onClickReverse = _ => {
    service.reverseInPlace()->ignore
  }
  let onClickExtend = _ => {
    service.extend()->ignore
  }
  React.useEffect0(() => {
    let intervalId = Js.Global.setInterval(onTick, 1000)
    Some(() => intervalId->Js.Global.clearInterval)
  })
  <div>
    <section> {React.string("State: " ++ state)} </section>
    <section>
      <button id="clickMeBtn" onClick=onClickReverse> {React.string("Reverse")} </button>
      <button id="clickMeBtn" onClick=onClickExtend> {React.string("Extend")} </button>
    </section>
  </div>
}
