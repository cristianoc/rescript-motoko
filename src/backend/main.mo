import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import StringHash "./StringHash";

actor Main {
  stable var gameState : Text = "";

  stable var map : StringHash.StringHash<Text> = StringHash.StringHash<Text>(10);

  public type PrincipalText = Text;

  public query func loadGameState(p:PrincipalText) : async Text {
    switch (StringHash.get(map, p)) {
      case (?state) { state };
      case null { "" };
    }
  };

  public func saveGameState(p:PrincipalText, s:Text) : async () {
    StringHash.put(map, p, s)
  };
};
