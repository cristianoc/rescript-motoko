let make = (~vel={Types.x: 0., y: 0.}, ~acc={Types.x: 0., y: 0.}, partType, px, py) => {
  let sprite = Sprite.makeParticle(partType)
  let lifetime = switch partType {
  | BrickChunkL | BrickChunkR => 300
  | GoombaSquish
  | Score100
  | Score200
  | Score400
  | Score800
  | Score1000
  | Score2000
  | Score4000
  | Score8000 => 30
  }
  {
    Types.sprite: sprite,
    lifetime: lifetime,
    px: px,
    py: py,
    vel: vel,
    acc: acc,
    kill: false,
  }
}

let makeScore = (score, pos) => {
  let t = switch score {
  | 100 => Types.Score100
  | 200 => Score200
  | 400 => Score400
  | 800 => Score800
  | 1000 => Score1000
  | 2000 => Score2000
  | 4000 => Score4000
  | 8000 => Score8000
  | _ => Score100
  }
  make(~vel={x: 0.5, y: -0.7}, t, pos)
}

// Mutably update the velocity of a particle
let updateVel = (part: Types.particle) => {
  part.vel.x = part.vel.x +. part.acc.x
  part.vel.y = part.vel.y +. part.acc.y
}

// Mutably update the position of a particle
let updatePos = (part: Types.particle) => {
  part.px = part.vel.x +. part.px
  part.py = part.vel.y +. part.py
}

let process = (part: Types.particle) => {
  part.lifetime = part.lifetime - 1
  if part.lifetime == 0 {
    part.kill = true
  }
  updateVel(part)
  updatePos(part)
}
