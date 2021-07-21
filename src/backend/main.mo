import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

actor Main {
  stable var map : Trie.Trie<Principal, Text> = Trie.empty();

  public query func loadGameState(p:Principal) : async Text {
    switch (Trie.find(map, key(p), Principal.equal)   ) {
      case (?state) { state };
      case null { "" };
    }
  };

  private func key(p : Principal) : Trie.Key<Principal> {
    return { hash = Principal.hash(p); key = p };
  };

  public func saveGameState(p:Principal, s:Text) : async () {
    map := Trie.put(map, key(p), Principal.equal, s).0;
  };

  type float = Float;
  type int = Int32;
 
  // Variant with only zero-ary cases
  type flatVariant = Nat32;
  
  // Variant with only unary cases whose payload is a flat variant
  type unaryFlatVariant = { tag: Text; value: flatVariant };
  
  type float2 = (float, float);

  type bool = Bool;


  type png = flatVariant;

  type spriteParams = {
    maxFrames: int;
    maxTicks: int;
    png: png;
    frameSize: float2;
    srcOffset: float2;
    bboxOffset: float2;
    bboxSize: float2;
  };

  type sprite = {
    params: spriteParams;
    frame: int;
    ticks: int;
  };

  type objTyp = unaryFlatVariant;

  type dir = flatVariant;

  type obj = {
    objTyp: objTyp;
    sprite: sprite;
    hasGravity: bool;
    speed: float;
    id: int;
    px: float; // x position
    py: float; // y position
    vx: float; // x velocity
    vy: float; // y velocity
    jumping: bool;
    grounded: bool;
    dir: dir;
    invuln: int;
    kill: bool;
    health: int;
    crouch: bool;
    score: int;
  };

  type xy = {
    x: float;
    y: float;
  };

  type particle = {
    sprite: sprite;
    lifetime: int;
    px: float;
    py: float;
    vel: xy;
    acc: xy;
    kill: bool;
  };

  type viewport = {
    px: float;
    py: float;
    v_dim: xy;
    m_dim: xy;
  };

  type state = {
    bgd: sprite;
    coins: int;
    level: int;
    multiplier: int;
    objects: [obj];
    particles: [particle];
    player1: obj;
    player2: obj;
    score: int;
    viewport: viewport;
  };

  stable var mapNative : Trie.Trie<Principal, state> = Trie.empty();

  public query func loadGameStateNative(p:Principal) : async (?state) {
    Trie.find(mapNative, key(p), Principal.equal)
  };

  public func saveGameStateNative(p:Principal, state:state) : async () {
    mapNative := Trie.put(mapNative, key(p), Principal.equal, state).0;
  };

};
