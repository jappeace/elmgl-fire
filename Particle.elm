module Particle exposing (..)
{-
A port of https://github.com/ethanhjennings/webgl-fire-particles
-}

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Hue
import Model exposing (..)
import Random exposing (Seed, initialSeed)
import VectorMath exposing (spread, random2DVec, toUnit, both)
import Noise exposing (PermutationTable, noise3d)

move : Float -> PermutationTable -> Float -> Particle -> Particle
move time table fps particle = let
    wind = Vec2.scale opts.windStrength <| toUnit <| both <| noise3d table ((Vec2.getX particle.position) / (toFloat opts.width)) ((Vec2.getY particle.position) / (toFloat opts.height)) time
  in
  {particle | 
    position = Vec2.add (Vec2.scale fps particle.velocity) particle.position,
    velocity = Vec2.add particle.velocity wind,
    color = Vec4.add particle.color (vec4 0.0 0.0 0.0 -(opts.fireDeathSpeed*fps)),
    size = particle.size - opts.fireShrinkFactor*fps
  }

logic : Float -> Model -> Random.Seed -> Model
logic fps model seed = 
    let 
        parts = createParticles seed model.particleDiscrepancy
    in
      {model | 
        particles = List.take 50 (List.map (move model.time model.permutTable fps) ((Tuple.second parts) ++ model.particles)), 
        particleDiscrepancy = Tuple.first parts + (toFloat opts.fireEmitRate) * fps
      }

createParticle : Seed -> (Particle, Seed)
createParticle seed = let
      size = spread seed opts.fireSize opts.fireSizeVarience
      speed = spread (Tuple.second size) opts.fireSpeed opts.fireSpeedVariance
      velocityrng = random2DVec (Tuple.second speed) (vec2 (pi/2) (pi/2)) opts.fireEmitVarience
      hue = spread (Tuple.second velocityrng) opts.fireTextureHue opts.fireTextureHueVariance
      position = random2DVec (Tuple.second hue) opts.fireEmitPosition opts.fireEmitSpread
      rgb = vec3 (Hue.convertHue <| Tuple.first hue) 1.0 1.0 |> Hue.hsvTorgb
    in
    ({
        size = Tuple.first size,
        velocity = (Vec2.scale (Tuple.first speed) (toUnit (Tuple.first velocityrng))),
        position = Tuple.first position,
        color = vec4 (Vec3.getX rgb) (Vec3.getY rgb) (Vec3.getZ rgb) 0.5
      }, Tuple.second position)

createParticles : Random.Seed -> Float -> (Float, List Particle)
createParticles seed discrepancy = if discrepancy <= 0 then (discrepancy, []) else 
    let
      result = createParticle seed
      other = createParticles (Tuple.second result) (discrepancy - 1)
    in
      (Tuple.first other, Tuple.first result :: Tuple.second other)
