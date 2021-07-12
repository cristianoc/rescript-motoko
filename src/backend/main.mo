import Nat "mo:base/Nat";

actor Main {
  type Tree = {
      #Leaf : (Nat);
      #Node : (Tree, Tree)
  };

  let initialTree = #Node(#Leaf(1), #Node(#Leaf(2), #Leaf(3)));

  stable var currTree : Tree = initialTree;
  stable var counter : Nat = 0;

  public query func get() : async Tree { currTree };

  public func set(t:Tree) : async () { currTree := t };

  public func reset() : async () { currTree := initialTree };

  func reverse_(t:Tree) : Tree {
    switch(t){
    case (#Leaf(_)) { t };
    case (#Node(t1,t2)) { #Node(reverse_(t2), reverse_(t1)) }
    };
  };

  public func reverse() : async () {
    currTree := reverse_(currTree)
  };

  public func extend() : async () {
    counter := counter + 1;
    currTree := #Node(#Leaf(counter),currTree);
  };
};
