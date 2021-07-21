/* TypeScript file generated from Types.res by genType. */
/* eslint-disable import/first */


import type {list} from '../../../src/frontend/src/shims/RescriptPervasives.shim';

// tslint:disable-next-line:interface-over-type-literal
export type loadOrSave = "Load" | "Save";

// tslint:disable-next-line:interface-over-type-literal
export type levelResult = "Won" | "Lost";

// tslint:disable-next-line:interface-over-type-literal
export type dir = "Left" | "Right";

// tslint:disable-next-line:interface-over-type-literal
export type xy = { x: number; y: number };

// tslint:disable-next-line:interface-over-type-literal
export type plTyp = "BigM" | "SmallM";

// tslint:disable-next-line:interface-over-type-literal
export type itemTyp = "Mushroom" | "Coin";

// tslint:disable-next-line:interface-over-type-literal
export type enemyTyp = 
    "Goomba"
  | "GKoopa"
  | "RKoopa"
  | "GKoopaShell"
  | "RKoopaShell";

// tslint:disable-next-line:interface-over-type-literal
export type blockTyp = 
    "QBlockMushroom"
  | "QBlockCoin"
  | "QBlockUsed"
  | "Brick"
  | "UnBBlock"
  | "Cloud"
  | "Panel"
  | "Ground";

// tslint:disable-next-line:interface-over-type-literal
export type status = 
    "Loading"
  | "Paused"
  | "Playing"
  | "Saving"
  | { tag: "LoggingIn"; value: loadOrSave }
  | { tag: "Finished"; value: { readonly levelResult: levelResult; readonly restartTime: number } };

// tslint:disable-next-line:interface-over-type-literal
export type png = 
    "Bgd1"
  | "Blocks"
  | "Chunks"
  | "Enemies"
  | "Ground"
  | "Items"
  | "MarioSmall"
  | "Mario2Small"
  | "MarioBig"
  | "Mario2Big"
  | "Panel"
  | "Score";

// tslint:disable-next-line:interface-over-type-literal
export type float2 = [number, number];

// tslint:disable-next-line:interface-over-type-literal
export type spriteParams = {
  readonly maxFrames: number; 
  readonly maxTicks: number; 
  readonly png: png; 
  readonly frameSize: float2; 
  readonly srcOffset: float2; 
  readonly bboxOffset: float2; 
  readonly bboxSize: float2
};

// tslint:disable-next-line:interface-over-type-literal
export type sprite = {
  params: spriteParams; 
  frame: number; 
  ticks: number
};

// tslint:disable-next-line:interface-over-type-literal
export type objTyp = 
    { tag: "Player1"; value: plTyp }
  | { tag: "Player2"; value: plTyp }
  | { tag: "Enemy"; value: enemyTyp }
  | { tag: "Item"; value: itemTyp }
  | { tag: "Block"; value: blockTyp };

// tslint:disable-next-line:interface-over-type-literal
export type obj = {
  objTyp: objTyp; 
  sprite: sprite; 
  hasGravity: boolean; 
  speed: number; 
  readonly id: number; 
  px: number; 
  py: number; 
  vx: number; 
  vy: number; 
  jumping: boolean; 
  grounded: boolean; 
  dir: dir; 
  invuln: number; 
  kill: boolean; 
  health: number; 
  crouch: boolean; 
  score: number
};

// tslint:disable-next-line:interface-over-type-literal
export type particle = {
  readonly sprite: sprite; 
  lifetime: number; 
  px: number; 
  py: number; 
  readonly vel: xy; 
  readonly acc: xy; 
  kill: boolean
};

// tslint:disable-next-line:interface-over-type-literal
export type viewport = {
  px: number; 
  py: number; 
  readonly v_dim: xy; 
  readonly m_dim: xy
};

// tslint:disable-next-line:interface-over-type-literal
export type state = {
  readonly bgd: sprite; 
  coins: number; 
  readonly level: number; 
  multiplier: number; 
  objects: list<obj>; 
  particles: list<particle>; 
  readonly player1: obj; 
  readonly player2: obj; 
  score: number; 
  status: status; 
  readonly viewport: viewport
};
