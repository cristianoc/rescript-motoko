/// Mutable hash map (aka Hashtable)
///
/// This module defines an imperative hash map (hash table), with a general key and value type.
///
/// It has a minimal object-oriented interface: `get`, `set`, `delete`, `count` and `entries`.
///
/// The class is parameterized by the key's equality and hash functions,
/// and an initial capacity.  However, as with the `Buffer` class, no array allocation
/// happens until the first `set`.
///
/// Internally, table growth policy is very simple, for now:
///  Double the current capacity when the expected bucket list size grows beyond a certain constant.

import Prim "mo:⛔";
import P "mo:base/Prelude";
import A "mo:base/Array";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import AssocList "mo:base/AssocList";
import Text "mo:base/Text";


module {


  // key-val list type
  type KVs<V> = AssocList.AssocList<Text, V>;

  public type StringHash<V> = {
    var table : [var KVs<V>];
    initCapacity : Nat;
    var _count : Nat;
  };

  public func StringHash<V>(initCapacity : Nat) : StringHash<V> {
    {var table = [var]; var _count = 0; initCapacity = initCapacity };
  };

  /// Returns the number of entries in this HashMap.
  public func size<V>(map:StringHash<V>) : Nat = map._count;

  /// Deletes the entry with the key `k`. Doesn't do anything if the key doesn't
  /// exist.
  public func delete<V>(map:StringHash<V>, k : Text) = ignore remove(map, k);


  /// Removes the entry with the key `k` and returns the associated value if it
  /// existed or `null` otherwise.
  public func remove<V>(map:StringHash<V>, k : Text) : ?V {
    let m = map.table.size();
    if (m > 0) {
      let h = Prim.nat32ToNat(Text.hash(k));
      let pos = h % m;
      let (kvs2, ov) = AssocList.replace<Text, V>(map.table[pos], k, Text.equal, null);
      map.table[pos] := kvs2;
      switch(ov){
        case null { };
        case _ { map._count -= 1; }
      };
      ov
    } else {
      null
    };
  };

  /// Gets the entry with the key `k` and returns its associated value if it
  /// existed or `null` otherwise.
  public func get<V>(map:StringHash<V>, k : Text) : ?V {
    let h = Prim.nat32ToNat(Text.hash(k));
    let m = map.table.size();
    let v = if (m > 0) {
      AssocList.find<Text, V>(map.table[h % m], k, Text.equal)
    } else {
      null
    };
  };

  /// Insert the value `v` at key `k`. Overwrites an existing entry with key `k`
  public func put<V>(map:StringHash<V>, k : Text, v : V) = ignore replace(map, k, v);

  /// Insert the value `v` at key `k` and returns the previous value stored at
  /// `k` or `null` if it didn't exist.
  public func replace<V>(map:StringHash<V>, k : Text, v : V) : ?V {
    if (map._count >= map.table.size()) {
      let size =
        if (map._count == 0) {
          if (map.initCapacity > 0) {
            map.initCapacity
          } else {
            1
          }
        } else {
          map.table.size() * 2;
        };
      let table2 = A.init<KVs<V>>(size, null);
      for (i in map.table.keys()) {
        var kvs = map.table[i];
        label moveKeyVals : ()
        loop {
          switch kvs {
            case null { break moveKeyVals };
            case (?((k, v), kvsTail)) {
              let h = Prim.nat32ToNat(Text.hash(k));
              let pos2 = h % table2.size();
              table2[pos2] := ?((k,v), table2[pos2]);
              kvs := kvsTail;
            };
          }
        };
      };
      map.table := table2;
    };
    let h = Prim.nat32ToNat(Text.hash(k));
    let pos = h % map.table.size();
    let (kvs2, ov) = AssocList.replace<Text, V>(map.table[pos], k, Text.equal, ?v);
    map.table[pos] := kvs2;
    switch(ov){
      case null { map._count += 1 };
      case _ {}
    };
    ov
  };

  /// Returns an iterator over the key value pairs in this
  /// `HashMap`. Does _not_ modify the `HashMap`.
  public func entries<V>(map:StringHash<V>) : Iter.Iter<(Text, V)> {
    if (map.table.size() == 0) {
      object { public func next() : ?(Text, V) { null } }
    }
    else {
      object {
        var kvs = map.table[0];
        var nextTablePos = 1;
        public func next () : ?(Text, V) {
          switch kvs {
            case (?(kv, kvs2)) {
              kvs := kvs2;
              ?kv
            };
            case null {
              if (nextTablePos < map.table.size()) {
                kvs := map.table[nextTablePos];
                nextTablePos += 1;
                next()
              } else {
                null
              }
            }
          }
        }
      }
    }
  };

  /// clone cannot be an efficient object method,
  /// ...but is still useful in tests, and beyond.
  public func clone<V> (
    h : StringHash<V>,
  ) : StringHash<V> {
    let h2 = StringHash<V>(size(h));
    for ((k,v) in entries(h)) {
      put(h2,k,v);
    };
    h2
  };

  /// Clone from any iterator of key-value pairs
  public func fromIter<V>(
    iter : Iter.Iter<(Text, V)>,
    initCapacity : Nat,
  ) : StringHash<V> {
    let h = StringHash<V>(initCapacity);
    for ((k, v) in iter) {
      put(h, k, v);
    };
    h
  };

  public func map<V1, V2>(
    h : StringHash<V1>,
    mapFn : (Text, V1) -> V2,
  ) : StringHash<V2> {
    let h2 = StringHash<V2>(size(h));
    for ((k, v1) in entries(h)) {
      let v2 = mapFn(k, v1);
      put(h2, k, v2);
    };
    h2
  };

  public func mapFilter<V1, V2>(
    h : StringHash<V1>,
    mapFn : (Text, V1) -> ?V2,
  ) : StringHash<V2> {
    let h2 = StringHash<V2>(size(h));
    for ((k, v1) in entries(h)) {
      switch (mapFn(k, v1)) {
        case null { };
        case (?v2) {
          put(h2, k, v2);
        };
      }
    };
    h2
  };

}