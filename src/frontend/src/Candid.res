module Principal: {
  type t
  let dummy: t
} = {
  type t = string
  let dummy = "dummy-principal"
}

module Actor = {
  type t = {
    loadGameState: (. Principal.t) => Js.Promise.t<string>,
    saveGameState: (. Principal.t, string) => Js.Promise.t<unit>,
  }
}
