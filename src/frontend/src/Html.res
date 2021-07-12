type imageElement = {
  @live
  mutable src: string,
}

type canvasRenderingContext2D = {
  clearRect: (. float, float, float, float) => unit,
  drawImage: (. imageElement, float, float, float, float, float, float, float, float) => unit,
  fill: (. unit) => unit,
  @live
  mutable fillStyle: string,
  fillText: (. string, float, float) => unit,
  @live
  mutable font: string,
  rect: (. float, float, float, float) => unit,
  scale: (. float, float) => unit,
  strokeRect: (. float, float, float, float) => unit,
  @live
  mutable strokeStyle: string,
}

type canvasElement = {
  getContext: (. string) => canvasRenderingContext2D,
  height: int,
  width: int,
}

@val external document: Dom.document = "document"

@val external window: Dom.window = "window"

type performance = {now: (. unit) => float}
@val external performance: performance = "performance"

/* external createImg: (_ [@bs.as "img"]) -> document -> imageElement = "createElement" [@@bs.send] */
@send external createImg: (Dom.document, @as("img") _) => imageElement = "createElement"

@val external requestAnimationFrame: (float => unit) => unit = "requestAnimationFrame"

@return(null_to_opt) @send
external getElementById: (Dom.document, string) => option<Dom.element> = "getElementById"

@send
external addEventListener: (Dom.document, string, Dom.event_like<'a> => bool, bool) => unit =
  "addEventListener"

@send
external addEventListenerImg: (imageElement, string, Dom.event_like<'a> => bool, bool) => unit =
  "addEventListener"

type renderingContext

external keyboardEventToJsObj: Dom.keyboardEvent => {..} = "%identity"

external elementToCanvasElement: Dom.element => canvasElement = "%identity"

external windowToJsObj: Dom.window => {..} = "%identity"
