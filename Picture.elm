module Picture exposing (..)

import Box exposing (..)
import Shape exposing (..)
import Style exposing (..)

type alias Rendering = List (Shape, Style)

type alias Picture = Box -> Rendering  

blank : Picture 
blank _ = []

times : Int -> (a -> a) -> (a -> a)
times n fn = 
  if n == 0 then identity 
  else fn >> (times (n - 1) fn)

turn : Picture -> Picture
turn p = turnBox >> p  

turns : Int -> (Picture -> Picture)
turns n = times n turn

flip : Picture -> Picture 
flip p = flipBox >> p 

toss : Picture -> Picture 
toss p = tossBox >> p 

aboveRatio : Int -> Int -> Picture -> Picture -> Picture 
aboveRatio m n p1 p2 = 
  \box -> 
    let 
      f = toFloat m / toFloat (m + n)
      (b1, b2) = splitVertically f box
    in 
      (p1 b1) ++ (p2 b2) 

above : Picture -> Picture -> Picture 
above = aboveRatio 1 1 

besideRatio : Int -> Int -> Picture -> Picture -> Picture 
besideRatio m n p1 p2 = 
  \box -> 
    let 
      f = toFloat m / toFloat (m + n)
      (b1, b2) = splitHorizontally f box
    in 
      (p1 b1) ++ (p2 b2) 

beside : Picture -> Picture -> Picture 
beside = besideRatio 1 1 

quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se = 
  above (beside nw ne)
        (beside sw se)

row : List Picture -> Picture 
row ps = 
  case ps of 
    [] -> blank
    [p] -> p 
    p::rest -> besideRatio 1 (List.length rest) p (row rest)

column : List Picture -> Picture 
column ps = 
  case ps of 
    [] -> blank
    [p] -> p 
    p::rest -> aboveRatio 1 (List.length rest) p (column rest)

nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture 
nonet nw nm ne mw mm me sw sm se = 
  [[nw,nm,ne], [mw,mm,me], [sw,sm,se]]
  |> List.map row
  |> column

over : Picture -> Picture -> Picture 
over p1 p2 =
  \box -> 
    (p1 box) ++ (p2 box) 

overs : List Picture -> Picture 
overs ps = 
  \box -> 
    List.concatMap (\p -> p box) ps

ttile : Picture -> Picture
ttile fish =
  let 
    fishN = fish |> toss |> flip
    fishE = fishN |> turns 3
  in 
    over fish (over fishN fishE)

utile : Picture -> Picture 
utile fish = 
  let 
    fishN = fish |> toss |> flip 
    fishW = turn fishN
    fishS = turn fishW
    fishE = turn fishS
  in
    overs [fishN, fishW, fishS, fishE]

side : Int -> Picture -> Picture 
side n fish = 
  let 
    s = if n == 1 then blank else side (n - 1) fish 
    t = ttile fish 
  in 
    quartet s s (turn t) t

corner : Int -> Picture -> Picture 
corner n fish = 
  let 
    s = if n == 1 then blank else side (n - 1) fish 
    c = if n == 1 then blank else corner (n - 1) fish 
  in 
    quartet c s (turn s) (utile fish)

squareLimit : Int -> Picture -> Picture
squareLimit n fish =
  let 
    c = corner n fish 
    s = side n fish 
    nw = c
    nm = s
    ne = c |> turns 3
    mw = s |> turn 
    mm = utile fish 
    me = s |> turns 3
    sw = c |> turn
    sm = s |> turns 2 
    se = c |> turns 2 
  in 
    nonet nw nm ne mw mm me sw sm se 