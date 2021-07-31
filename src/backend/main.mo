import Array "mo:base/Array";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

actor Main {

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
    date: Float;
    coins: int_;
    idCounter:int_;
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

  stable var map : Trie.Trie<Principal, delta> = Trie.empty();

  type highScore = {date:Float; principal:Principal; score:int_};
  stable var scores : [var ?highScore] = Array.init(6, null);

  private func shiftRightScore(pos:Nat) {
    if (pos < 0 or pos >= scores.size()) return;
    for (i in Iter.revRange(scores.size()-2, pos)) {
      let n = Int.abs(i);
      scores[n+1] := scores[n];
    };
    scores[pos] := null;
  };

  private func shiftLeftScore(pos:Nat) {
    if (pos < 0 or scores.size() == 0) return;
    for (i in Iter.range(pos, scores.size()-2)) {
      scores[i] := scores[i+1];
    };
    scores[scores.size()-1] := null;
  };

  private func removeScore(principal:Principal, date:Float) {
    for (i in scores.keys()) {
      switch (scores[i]) {
        case null {
          return;
        };
        case (?slot) {
          if (slot.date == date and Principal.equal(slot.principal, principal)) {
            shiftLeftScore(i);
            return;
          };
        };
      };
    };
  };

  private func addScore(principal:Principal, date:Float, score:int_) {
    for (i in scores.keys()) {
      switch (scores[i]) {
        case null {
          scores[i] := ?{date; principal; score};
          return;
        };
        case (?slot) {
          if (score > slot.score) {
            shiftRightScore(i);
            scores[i] := ?{date; principal; score};
            return;
          };
        };
      };
    };
  };

  private func updateScore(principal:Principal, date:Float, score:int_) {
    removeScore(principal, date);
    addScore(principal, date, score);
  };

  private func pKey(p : Principal) : Trie.Key<Principal> {
    return { hash = Principal.hash(p); key = p };
  };

  public query func loadDelta(p:Principal) : async (?delta) {
    Trie.find(map, pKey(p), Principal.equal)
  };

  public func saveDelta(principal:Principal, delta:delta) : async () {
    map := Trie.put(map, pKey(principal), Principal.equal, delta).0;
    updateScore(principal, delta.state.date, delta.state.score);
  };

  public query func highScores(): async ([{name:Text; score:int_}]) {
    func filter (x:?highScore) : ?{name:Text; score:int_} {
      switch x {
        case null {
          null
        };
        case (?hs) {
          ?{name=Principal.toText(hs.principal); score=hs.score}
        }
      };
    };
    Array.mapFilter(Array.freeze(scores), filter);
  };
};
