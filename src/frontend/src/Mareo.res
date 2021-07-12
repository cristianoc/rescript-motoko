open Belt

module Html = Html

// Used for concurrency issues.
let preload = () => {
  let loadCount = ref(0)
  let numImages = Config.images->Array.length
  Config.images->Array.forEachU((. img_src) => {
    let img = Html.createImg(Html.document)
    img.src = Config.spritesDir ++ img_src
    img->Html.addEventListenerImg(
      "load",
      _ => {
        loadCount := loadCount.contents + 1
        if loadCount.contents == numImages {
          Director.updateLoop()
        }
        true
      },
      true,
    )
  })
}

@live
let start = () => {
  Html.windowToJsObj(Html.window)["onload"] = _ => {
    preload()
    Js.Global.setInterval(Backend.onTick, 1000)->ignore
    true
  }
}
