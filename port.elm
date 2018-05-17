module Port exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Task
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture as Texture exposing (Error, Texture)

type alias Particle = {
    size : Float,
    velocity: Vec2,
    postition: Vec2,
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
    omnidirectionalWind: Bool
}

opts : Options
opts = {
    fireEmitPositionSpread = vec2 100 20,
    fireEmitRate = 1600,
    fireSize = 40.0,
    fireSizeVarience = 100.0,
    fireEmitVarience = 100.0,
    fireSpeed = 200,
    fireDeathSpeed = 0.003,
    fireTriangleness =  0.00015,
    fireTextureHue =  25.0,
    fireTextureHueVariance = 15.0,
    fireTextureColorize = True,
    wind = True,
    omnidirectionalWind = False
  }

type alias Model =
    { texture : Maybe Texture
    , theta : Float
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 10000 }, Cmd.none )

init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing, theta = 0 }
    , Task.attempt TextureLoaded (Texture.load "texture/flame.png")
    )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Animate)
        , update = update
        }

view : Model -> Html Msg
view { texture, theta } =
    Html.div [] [
    Html.p [] [Html.text (toString theta)],
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.stencil 0
        ]
        [ width 400
        , height 400
        , style [ ( "display", "block" ) ]
        ]
        (texture
            |> Maybe.map (scene (perspective theta))
            |> Maybe.withDefault []
        )
      ]
