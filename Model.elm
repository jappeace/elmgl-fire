module Model exposing (..)
{-
Dumb global type definitions used in lots of places
-}

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import WebGL.Texture as Texture exposing (Error, Texture)
import Time exposing (Time)
import Random
import Noise exposing (PermutationTable)

type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Time
    | Entropy Int

type alias Particle = {
    size : Float,
    velocity: Vec2,
    position: Vec2,
    color: Vec4 
}

type alias Model =
    { texture : Maybe Texture
    , theta : Float
    , options : Options
    , particles : List Particle
    , entropy: Maybe Random.Seed
    , particleDiscrepancy: Float
    , permutTable: PermutationTable --
    }

type alias Options = {
    fireEmitPosition: Vec2,
    fireEmitSpread: Float,
    fireEmitRate: Int,
    fireSize: Float,
    fireSizeVarience: Float,
    fireEmitVarience: Float,
    fireSpeed: Float,
    fireSpeedVariance: Float,
    fireDeathSpeed: Float,
    fireTriangleness: Float,
    fireTextureHue: Float,
    fireTextureHueVariance: Float,
    fireTextureColorize: Bool,
    wind: Bool,
    omnidirectionalWind: Bool,
    width: Int,
    height: Int
}

opts : Options
opts = let 
    width = 400
    height = width
  in
  {
    fireEmitPosition = vec2 (width/2) (height/2+200),
    fireEmitSpread = 100,
    fireEmitRate = 30,
    fireSize = 40.0,
    fireSizeVarience = 10.0, -- variation up and down from firesize in px
    fireEmitVarience = 1.0,
    fireSpeed = 200,
    fireDeathSpeed = 0.003,
    fireTriangleness =  0.00015,
    fireTextureHue =  25.0,
    fireTextureHueVariance = 15.0,
    fireTextureColorize = True,
    wind = True,
    omnidirectionalWind = False,
    width = width,
    height = height,
    fireSpeedVariance = 4.0
  }
