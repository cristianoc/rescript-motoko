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
  type int_ = Int32;

  // Variant with only zero-ary cases
  type flatVariant = Nat32;
  
  // Variant with only unary cases whose payload is a flat variant
  type unaryFlatVariant = { TAG: Nat32; _0: flatVariant };
  
  type float2 = (float, float);

  type bool = Bool;


  type png = flatVariant;

  type spriteParams = {
    maxFrames: int_;
    maxTicks: int_;
    png: png;
    frameSize: float2;
    srcOffset: float2;
    bboxOffset: float2;
    bboxSize: float2;
  };

  type sprite = {
    params: spriteParams;
    frame: int_;
    ticks: int_;
  };

  type objTyp = unaryFlatVariant;

  type dir = flatVariant;

  type obj = {
    objTyp: objTyp;
    sprite: sprite;
    hasGravity: bool;
    speed: float;
    id: int_;
    px: float; // x position
    py: float; // y position
    vx: float; // x velocity
    vy: float; // y velocity
    jumping: bool;
    grounded: bool;
    dir: dir;
    invuln: int_;
    kill: bool;
    health: int_;
    crouch: bool;
    score: int_;
  };

  type xy = {
    x: float;
    y: float;
  };

  type particle = {
    sprite: sprite;
    lifetime: int_;
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
    coins: int_;
    level: int_;
    multiplier: int_;
    objects: [obj];
    particles: [particle];
    player1: obj;
    player2: obj;
    score: int_;
    viewport: viewport;
  };

  type delta = {
    missing: [int_];
    state: state;
  };

  stable var mapNative : Trie.Trie<Principal, delta> = Trie.empty();

  public query func loadGameStateNative(p:Principal) : async (?delta) {
    Trie.find(mapNative, key(p), Principal.equal)
  };

  public func saveGameStateNative(p:Principal, delta:delta) : async () {
    mapNative := Trie.put(mapNative, key(p), Principal.equal, delta).0;
  };

};
