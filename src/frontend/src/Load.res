type canvasData = {
  canvasElement: Html.canvasElement,
  sizeScaled: (float, float),
  context: Html.canvasRenderingContext2D,
}

let canvasAndContext = lazy (
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
      canvasElement: canvasElement,
      context: context,
      sizeScaled: (width /. Config.scale, height /. Config.scale),
    }
  }
)

let getCanvasAndContext = () => Lazy.force(canvasAndContext)

let getCanvas = () => getCanvasAndContext().canvasElement

let getContext = () => getCanvasAndContext().context

let getCanvasSizeScaled = () => getCanvasAndContext().sizeScaled
