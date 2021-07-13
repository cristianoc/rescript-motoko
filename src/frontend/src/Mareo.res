@live
let start = () => {
  Html.windowToJsObj(Html.window)["onload"] = _ => {
    Director.updateLoop()
    Js.Global.setInterval(Backend.onTick, 1000)->ignore
    true
  }
}
