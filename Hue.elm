module Hue exposing (..)
{-| 
  Functions for dealing with hue based colors
-}
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

-- convert from degrees to 0..1 scale (not really cause it doesn't do modulo)
convertHue : Float -> Float
convertHue hue = 
    let
      result = hue / 360.0
    in
        if result < 0 then result + 1.0 else result

-- some deep magick to go from hue saturation value to rgb
hsvTorgb: Vec3 -> Vec3
hsvTorgb hsv = 
  let
      h = Vec3.getX hsv
      s = Vec3.getY hsv
      v = Vec3.getZ hsv
      i = floor (h * 6) -- TODO micro optimization because eager
      f = h*6 - toFloat i
      p = v * (1-s)
      q = v * (1-f*s)
      t = v * (1-(1-f)*s)
  in
    case i of
        0 -> vec3 v t p
        1 -> vec3 q v p
        2 -> vec3 p v t
        3 -> vec3 p q v
        4 -> vec3 t p v
        5 -> vec3 v p q
        _ -> hsv
