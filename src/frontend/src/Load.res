type sizeScaled = {widthScaled: float, heightScaled: float}

type canvasData = {
  sizeScaled: sizeScaled,
  context: Html.canvasRenderingContext2D,
}

let lazyCanvasData = lazy (
  switch Html.getElementById(Html.document, Config.canvasId) {
  | None =>
    print_endline("cant find canvas " ++ (Config.canvasId ++ " \n"))
    assert false
  | Some(el) =>
    let canvasElement = Html.elementToCanvasElement(el)
    let width = float_of_int(canvasElement.width)
    let height = float_of_int(canvasElement.height)
    let context = canvasElement.getContext(. "2d")
    context.scale(. Config.scale, Config.scale)
    Html.addEventListener(Html.document, "keydown", Keys.keydown, true)
    Html.addEventListener(Html.document, "keyup", Keys.keyup, true)
    {
      context: context,
      sizeScaled: {widthScaled: width /. Config.scale, heightScaled: height /. Config.scale},
    }
  }
)

let getCanvasData = () => Lazy.force(lazyCanvasData)

let getContext = () => getCanvasData().context

let getSizeScaled = () => getCanvasData().sizeScaled
