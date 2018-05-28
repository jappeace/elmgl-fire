module Port exposing (..)
{-
A port of https://github.com/ethanhjennings/webgl-fire-particles
-}

import AnimationFrame
import Html exposing (Html)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Task
import WebGL.Texture as Texture exposing (Error, Texture)
import Hue
import Model exposing (..)
import View exposing (view)
import Random exposing (Seed)

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )
        Animate dt ->
            let 
                newModel = { model |
                  theta = model.theta + dt / 1000, -- always count
                  entropy = Nothing -- don't re-use entropy
                }
            in
            ( 
              Maybe.map (logic (dt / 1000) newModel) 
                model.entropy |> -- maybe entropy
                Maybe.withDefault newModel, 
                getEntropy -- always more entropy
            )
        Entropy value -> ({model | entropy = Just <| Random.initialSeed value}, Cmd.none)

getEntropy : Cmd Msg
getEntropy = Random.generate Entropy <| Random.int Random.minInt Random.maxInt

randomFloat : Seed -> (Float, Seed)
randomFloat = Random.step (Random.float 0 1)

logic : Float -> Model -> Random.Seed -> Model
logic fps model seed = 
    let 
        parts = createParticles seed model.particleDiscrepancy
    in
      {model | 
        particles = (Tuple.second parts) ++ model.particles, 
        particleDiscrepancy = Tuple.first parts + (toFloat opts.fireEmitRate) * fps
      }

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
     
toUnit : Vec2 -> Vec2
toUnit vec = let
      normal = Vec2.normalize vec
    in
     vec2 ((cos (Vec2.getX normal))*0.2) (-(sin (Vec2.getY normal)))

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

init : ( Model, Cmd Msg )
init =
    ( {
        texture = Nothing,
        theta = 0,
        options = opts,
        particles = [],
        entropy = Nothing,
        particleDiscrepancy = 0.0
    }
    , Task.attempt TextureLoaded (Texture.loadWith { 
        magnify = Texture.linear, 
        minify = Texture.linear, 
        horizontalWrap = Texture.repeat,
        verticalWrap = Texture.repeat, 
        flipY = True
      } "texture/gradient.png")
    )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Animate)
        , update = update
        }

