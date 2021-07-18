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
};
