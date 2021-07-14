@live
let start = () => {
  Html.windowToJsObj(Html.window)["onload"] = _ => {
    Director.updateLoop()
    true
  }
}
