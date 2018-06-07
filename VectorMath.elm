module VectorMath exposing (..)
{-
A port of https://github.com/ethanhjennings/webgl-fire-particles
-}

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random exposing (Seed, initialSeed)

randomFloat : Seed -> (Float, Seed)
randomFloat = Random.step (Random.float 0 1)

spread : Seed -> Float -> Float -> (Float, Seed)
spread seed center variance = let
      rngFloat = randomFloat seed
    in
      (((Tuple.first rngFloat) - 0.5) * variance + center, Tuple.second rngFloat)

random2DVec : Seed -> Vec2 -> Float -> (Vec2, Seed)
random2DVec seed origin spreadAmount = let
      x = spread seed (Vec2.getX origin) spreadAmount 
      y = spread (Tuple.second x) (Vec2.getY origin) spreadAmount
    in
        (vec2 (Tuple.first x) (Tuple.first y), Tuple.second y)
     
randomUnitVec : Seed -> Float -> Float -> (Vec2, Seed)
randomUnitVec seed origin spreadAmount = let
    angle = spread seed origin spreadAmount 
      in 
    (angleVec (Tuple.first angle), Tuple.second angle)

one = vec2 1.0 1.0

both : Float -> Vec2
both sd = Vec2.scale sd one

angleVec : Float -> Vec2
angleVec angle = 
     vec2 (cos angle) (-(sin angle))
