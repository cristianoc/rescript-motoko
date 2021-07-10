module Tree = {
  type raw
  type rec t = Leaf(int) | Node(t, t)

  let rec fromCandid: raw => t = raw => {
    let t = Obj.magic(raw)
    t["Leaf"] !== Js.undefined
      ? Leaf(t["Leaf"])
      : {
          let (x, y) = t["Node"]
          Node(fromCandid(x), fromCandid(y))
        }
  }

  let rec toCandid: t => raw = t =>
    switch t {
    | Leaf(n) => {"Leaf": n}->Obj.magic
    | Node(x, y) => {"Node": (x->toCandid, y->toCandid)}->Obj.magic
    }
}

module Actor = {
  type t = {
    extend: (. unit) => Js.Promise.t<unit>,
    get: (. unit) => Js.Promise.t<Tree.raw>,
    set: (. Tree.raw) => Js.Promise.t<unit>,
    reverseInPlace: (. unit) => Js.Promise.t<unit>,
    reset: (. unit) => Js.Promise.t<unit>,
  }
}

module Service = {
  type t = {
    extend: unit => Js.Promise.t<unit>,
    get: unit => Js.Promise.t<Tree.t>,
    set: Tree.t => Js.Promise.t<unit>,
    reverseInPlace: unit => Js.Promise.t<unit>,
    reset: unit => Js.Promise.t<unit>,
  }

  let fromActor = (actor: Actor.t): t => {
    extend: () => actor.extend(.),
    get: () => actor.get(.)->Promise.thenResolve(raw => raw->Tree.fromCandid),
    set: t => actor.set(. t->Tree.toCandid),
    reverseInPlace: () => actor.reverseInPlace(.),
    reset: () => actor.reset(.),
  }
}
