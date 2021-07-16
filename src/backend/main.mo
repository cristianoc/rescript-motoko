import Text "mo:base/Text";
import Trie "mo:base/Trie";

actor Main {
  stable var gameState : Text = "";

  stable var map : Trie.Trie<Text, Text> = Trie.empty();

  public type PrincipalText = Text;

  public query func loadGameState(p:PrincipalText) : async Text {
    switch (Trie.find(map, key(p), Text.equal)   ) {
      case (?state) { state };
      case null { "" };
    }
  };

  private func key(p : PrincipalText) : Trie.Key<PrincipalText> {
    return { hash = Text.hash(p); key = p };
  };

  public func saveGameState(p:PrincipalText, s:Text) : async () {
    map := Trie.put(map, key(p), Text.equal, s).0;
  };
};
