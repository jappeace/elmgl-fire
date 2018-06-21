module Port exposing (..)
{-
A port of https://github.com/ethanhjennings/webgl-fire-particles
-}

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import AnimationFrame
import Html exposing (Html)
import Task
import WebGL.Texture as Texture exposing (Error, Texture)
import Model exposing (..)
import View exposing (view)
import Random exposing (Seed, initialSeed)
import Noise exposing (noise3d, permutationTable )
import Particle exposing (logic)

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            let 
                uniforms = Maybe.map (\text -> {
                    texture=text, 
                    time_now = 0.0,
                    resolution= vec2 (toFloat opts.width) (toFloat opts.height)
                  } ) <| Result.toMaybe textureResult 
            in
            ({ model | uniforms = uniforms} , Cmd.none )
        Animate dt ->
            if dt > opts.maxFrameInterval then (model, Cmd.none) else
            let 
                newTime = model.time+ dt / 1000 -- always count
                newModel = { model |
                  time = newTime, 
                  uniforms = Maybe.map (\x -> {x| time_now=newTime}) model.uniforms,
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
        uniforms = Nothing,
        time = 0,
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

