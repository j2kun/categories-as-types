use "sets.sml";

exception uncomposable
exception invalidArrow 

datatype ('obj, 'arrow)Category = 
   category of ('arrow -> 'obj) * 
               ('arrow -> 'obj) * 
               ('obj -> 'arrow) * 
               ('arrow * 'arrow -> 'arrow)


datatype 'a SetMap = setMap of ('a Set) * ('a -> 'a) * ('a Set)

fun setSource(setMap(a, f, b)) = a
fun setTarget(setMap(a, f, b)) = b
fun setIdentity(aSet) = setMap(aSet, (fn x => x), aSet)
fun setCompose(setMap(b', g, c), setMap(a, f, b)) = 
      if setEq(b)(b') then 
         setMap(a, (g o f), c)
      else
         raise uncomposable


val FiniteSets = category(setSource, setTarget, setIdentity, setCompose)


datatype 'a PosetArrow = ltArrow of 'a * ('a -> 'a -> bool) * 'a

fun setPosetArrow(x)(y) = if subset(x)(y) then ltArrow(x, subset, y)
                          else raise invalidArrow


fun posetSource(ltArrow(x, f, y)) = x
fun posetTarget(ltArrow(x, f, y)) = y
fun posetIdentity(x) = ltArrow(x, (fn z => fn w => true), x)
fun posetCompose(ltArrow(b', g, c), ltArrow(a, f, b)) = 
      if b = b' then 
         ltArrow(a, (fn x => fn y => f(x)(b) andalso g(b)(y)), c)
      else
         raise uncomposable

val Posets = category(posetSource, posetTarget, posetIdentity, posetCompose)


type 'a PosetCategory = ('a Set, ('a Set)PosetArrow)Category



datatype 'a TriangleDiagram = triangle of ('a SetMap) * ('a -> 'a) * ('a SetMap)

fun makeArrow(fixedSet)(f)(target) = setMap(fixedSet, f, target)

type 'a TriangleDiagramCategory = 
   (('a SetMap), ('a TriangleDiagram))Category


fun triangleSource(triangle(x, f, y)) = x
fun triangleTarget(triangle(x, f, y)) = y
fun triangleIdentity(x) = triangle(x, (fn z => z), x)
fun triangleCompose(triangle(b', g, c), triangle(a, f, b)) =
      if setEq(setTarget(b'))(setTarget(b)) then
         triangle(a, (g o f), c)
      else
         raise uncomposable

val SetDiagrams = 
   category(triangleSource, triangleTarget, triangleIdentity, triangleCompose)
