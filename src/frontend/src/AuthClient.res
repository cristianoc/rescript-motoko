type t // agent

type identity = {getPrincipal: (. unit) => Backend.Candid.principal}

type authClient = {
  login: (. {"onSuccess": (. unit) => unit, "onError": (. string) => unit}) => Promise.t<unit>,
  getIdentity: (. unit) => identity,
}

@module("@dfinity/auth-client")
external _AuthClient: {"create": (. unit) => Promise.t<authClient>} = "AuthClient"

module Error = {
  type t = LoginError(string) | PromiseError(exn) | Timeout(float)
  let toString = e =>
    switch e {
    | LoginError(s) => "Authorization login error:" ++ s
    | PromiseError(_exn) => "Authorization promise error"
    | Timeout(n) => "Authorization timeout:" ++ Js.Float.toString(n) ++ " seconds"
    }
}

let authenticate = (~onSuccess, ~onError, ~timeoutInSeconds) => {
  let timeoutId = Js.Global.setTimeoutFloat(
    () => onError(Error.Timeout(timeoutInSeconds)),
    timeoutInSeconds *. 1000.,
  )
  _AuthClient["create"](.)
  ->Promise.then(authClient => {
    authClient.login(. {
      "onSuccess": (. ()) => {
        Js.Global.clearTimeout(timeoutId)
        let principal = authClient.getIdentity(.).getPrincipal(.)
        onSuccess(~principal)
      },
      "onError": (. error) => {
        Js.Global.clearTimeout(timeoutId)
        onError(LoginError(error))
      },
    })
  })
  ->Promise.catch(exn => {
    Js.Global.clearTimeout(timeoutId)
    onError(PromiseError(exn))
    Promise.resolve()
  })
}
