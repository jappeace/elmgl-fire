module Port exposing (..)
{-
A port of https://github.com/ethanhjennings/webgl-fire-particles
-}

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Task
import WebGL.Texture as Texture exposing (Error, Texture)
import Hue
import Model exposing (..)
import View exposing (view)

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )
        Animate dt ->
            ( logic dt { model | theta = model.theta + dt / 10000 }, Cmd.none )

logic : Float -> Model -> Model
logic fps model = model -- TODO port logic

init : ( Model, Cmd Msg )
init =
    let 
        rgb = vec3 (Hue.convertHue opts.fireTextureHue) 1.0 1.0 |> Hue.hsvTorgb
    in
    ( { texture = Nothing, theta = 0, options = opts, particles = [
        {
          size = opts.fireSize,
          velocity = vec2 10.0 10.0,
          position = vec2 200.0 20.0,
          color = vec4 (Vec3.getX rgb) (Vec3.getY rgb) (Vec3.getZ rgb) 0.8
        }
    ]}
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

