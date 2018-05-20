module Model exposing (..)
{-
Dumb global type definitions used in lots of places
-}

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import WebGL.Texture as Texture exposing (Error, Texture)
import Time exposing (Time)

type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Time


type alias Particle = {
    size : Float,
    velocity: Vec2,
    position: Vec2,
    color: Vec4 
}

type alias Options = {
    fireEmitPositionSpread: Vec2,
    fireEmitRate: Int,
    fireSize: Float,
    fireSizeVarience: Float,
    fireEmitVarience: Float,
    fireSpeed: Float,
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
opts = {
    fireEmitPositionSpread = vec2 100 20,
    fireEmitRate = 1600,
    fireSize = 40.0,
    fireSizeVarience = 1.0,
    fireEmitVarience = 100.0,
    fireSpeed = 200,
    fireDeathSpeed = 0.003,
    fireTriangleness =  0.00015,
    fireTextureHue =  25.0,
    fireTextureHueVariance = 15.0,
    fireTextureColorize = True,
    wind = True,
    omnidirectionalWind = False,
    width = 400,
    height = 400
  }

type alias Model =
    { texture : Maybe Texture
    , theta : Float
    , options : Options
    , particles : List Particle
    }
