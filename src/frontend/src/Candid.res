type principal

module Actor = {
  type t = {
    loadGameState: (. principal) => Js.Promise.t<string>,
    saveGameState: (. principal, string) => Js.Promise.t<unit>,
  }
}
