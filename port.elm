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
import Random exposing (Seed, initialSeed)
import Noise exposing (noise3d, permutationTable )
import Particle exposing (logic)

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

init : ( Model, Cmd Msg )
init =
    ( {
        texture = Nothing,
        theta = 0,
        options = opts,
        particles = [],
        entropy = Nothing,
        particleDiscrepancy = 0.0,
        permutTable = Tuple.first (permutationTable (initialSeed 42))
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

