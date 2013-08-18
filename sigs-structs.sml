
structure OrderedInt : ORD_KEY = 
struct
   type ord_key = int
   val compare = Int.compare
end

signature MAG_OBJ = 
sig
   type object
   val mag : object -> int
end

signature DIST_OBJ = 
sig
   type object
   val dist: object * object -> int
end

functor MakeMagSet(structure ELT: ORD_KEY) = 
struct
   structure Set : ORD_SET = RedBlackSetFn(ELT)
   type object = Set.set 
   val mag = Set.numItems
end

functor MakeDistObj(structure MAG: MAG_OBJ) = 
struct
   type object = MAG.object 
   val dist = fn (x, y) => 
               let 
                  val v = MAG.mag(x) - MAG.mag(y)
               in
                  if v > 0 then v else ~v
               end
end

structure IntMagSet = MakeMagSet(structure ELT = OrderedInt)
val X = IntMagSet.Set.addList(IntMagSet.Set.empty, [0,1,2,3,4,5,6,7,8,9])
val xMag = IntMagSet.mag(X) 

structure IntSetDist = MakeDistObj(structure MAG = IntMagSet)
open IntSetDist
val z = dist(X, X)
