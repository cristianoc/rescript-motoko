open Actors

type t = {
  mutable px: float,
  mutable py: float,
  v_dim: Actors.xy,
  m_dim: Actors.xy,
}

let make = ((vx, vy), (mx, my)) => {
  px: 0.,
  py: 0.,
  v_dim: {
    x: vx,
    y: vy,
  },
  m_dim: {
    x: mx,
    y: my,
  },
}

// Calculate the viewport origin coordinate given the centering coordinate
// [cc], the canvas coordinate [vc], and the map coordinate [mc]. This function
// works for both x and y. At the extreme points, it will ensure that the
// viewport is always within bounds of the map, even if it is no longer
// centered about the origin point.
let calcViewportPoint = (cc, vc, mc) => {
  let vc_half = vc /. 2.
  min(max(cc -. vc_half, 0.), min(mc -. vc, abs_float(cc -. vc_half)))
}

// Return whether a coordinate pair [pos] is inside the viewport [v]
let inViewport = (v, px, py) => {
  let margin = 32.
  let (v_min_x, v_max_x) = (v.px -. margin, v.px +. v.v_dim.x)
  let (v_min_y, v_max_y) = (v.py -. margin, v.py +. v.v_dim.y)
  px >= v_min_x && (px <= v_max_x && (py >= v_min_y && py <= v_max_y))
}

// Return whether an object is outside of the viewport and below it. This is
// useful for determining whether to process falling out of screen normally.
let outOfViewportBelow = (v, y) => {
  let vMaxY = v.v_dim.y *. 1.5
  y +. 20.0 >= vMaxY
}

// Convert a x,y [coord] pair in absolute coordinates to coordinates relative
// to the viewport
let fromCoord = (viewport, px, py) => {
  x: px -. viewport.px,
  y: py -. viewport.py,
}

// Update the viewport [vpt] given the new center x,y coordinate pair [ctr]
let update = (vpt, px, py) => {
  let newX = calcViewportPoint(px, vpt.v_dim.x, vpt.m_dim.x)
  let newY = calcViewportPoint(py, vpt.v_dim.y, vpt.m_dim.y)
  vpt.px = newX
  vpt.py = newY
}
