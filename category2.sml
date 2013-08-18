
signature CATEGORY = 
sig
   type object
   type arrow
   exception uncomposable

   val source: arrow -> object
   val target: arrow -> object
   val identity: object -> arrow
   val composable: arrow * arrow -> bool
   val compose: arrow * arrow -> arrow 
end


functor MakeSetCategory(structure ELT: ORD_KEY) = 
struct
   structure Set : ORD_SET = RedBlackSetFn(ELT)
   type elt = ELT.ord_key
   type object = Set.set 
   datatype arrow = function of object * (elt -> elt) * object
   
   exception uncomposable

   fun source(function(a, _, _)) = a 
   fun target(function(_, _, b)) = b 
   fun identity(a) = function(a, fn x => x, a)
   fun composable(a,b) = Set.equal(a,b)
   fun compose(function(b2, g, c), function(a, f, b1)) = 
      if composable(b1, b2) then
         function(a, (g o f), c)
      else
         raise uncomposable
   fun apply(function(a, f, b), x) = f(x)
end


structure OrderedInt : ORD_KEY = 
struct
   type ord_key = int
   val compare = Int.compare
end


structure IntSets = MakeSetCategory(structure ELT = OrderedInt)


open IntSets
val X = Set.addList(Set.empty, [0,1,2,3,4,5,6,7,8,9])
val Y = Set.addList(Set.empty, [0,1,2])
val F = function(X, fn x => x mod 2, Y)
val G = function(Y, fn x => 2*x, Y)

val fsrc = Set.listItems(source(F))
val ftarget = Set.listItems(target(F))
val idx = identity(X)
val comp = compose(G, F)
val gfx = apply(comp, 5)
