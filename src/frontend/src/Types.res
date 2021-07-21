type loadOrSave = Load | Save

type levelResult =
  | Won
  | Lost

type dir =
  | Left
  | Right

type dir2 =
  | North
  | South
  | East
  | West

type xy = {
  mutable x: float,
  mutable y: float,
}

type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

type plTyp =
  | BigM
  | SmallM

type itemTyp =
  | Mushroom
  | Coin

type enemyTyp =
  | Goomba
  | GKoopa
  | RKoopa
  | GKoopaShell
  | RKoopaShell

type blockTyp =
  | QBlockMushroom
  | QBlockCoin
  | QBlockUsed
  | Brick
  | UnBBlock
  | Cloud
  | Panel
  | Ground

type playerTyp =
  | Standing
  | Jumping
  | Running
  | Crouching

type playerNum =
  | One
  | Two

type partTyp =
  | GoombaSquish
  | BrickChunkL
  | BrickChunkR
  | Score100
  | Score200
  | Score400
  | Score800
  | Score1000
  | Score2000
  | Score4000
  | Score8000

type png =
  | Bgd1
  | Blocks
  | Chunks
  | Enemies
  | Ground
  | Items
  | MarioSmall
  | Mario2Small
  | MarioBig
  | Mario2Big
  | Panel
  | Score

type float2 = (float, float)

type spriteParams = {
  maxFrames: int,
  maxTicks: int,
  png: png,
  frameSize: float2,
  srcOffset: float2,
  bboxOffset: float2,
  bboxSize: float2,
}

type sprite = {
  mutable params: spriteParams,
  mutable frame: int,
  mutable ticks: int,
}

type objTyp =
  | Player1(plTyp)
  | Player2(plTyp)
  | Enemy(enemyTyp)
  | Item(itemTyp)
  | Block(blockTyp)

type obj = {
  mutable objTyp: objTyp,
  mutable sprite: sprite,
  mutable hasGravity: bool,
  mutable speed: float,
  id: int,
  mutable px: float, // x position
  mutable py: float, // y position
  mutable vx: float, // x velocity
  mutable vy: float, // y velocity
  mutable jumping: bool,
  mutable grounded: bool,
  mutable dir: dir,
  mutable invuln: int,
  mutable kill: bool,
  mutable health: int,
  mutable crouch: bool,
  mutable score: int,
}

@genType
type particle = {
  sprite: sprite,
  mutable lifetime: int,
  mutable px: float,
  mutable py: float,
  vel: xy,
  acc: xy,
  mutable kill: bool,
}

type viewport = {
  mutable px: float,
  mutable py: float,
  v_dim: xy,
  m_dim: xy,
}

// State of the game
@genType
type state = {
  bgd: sprite,
  mutable coins: int,
  level: int,
  mutable multiplier: int,
  mutable objects: list<obj>,
  mutable particles: array<particle>,
  player1: obj,
  player2: obj,
  mutable score: int,
  viewport: viewport,
}
