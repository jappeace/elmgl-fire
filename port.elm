module Port exposing (..)
{-
A port of https://github.com/ethanhjennings/webgl-fire-particles
-}

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
import WebGL.Texture as Texture exposing (Error, Texture)
import WebGL.Settings.Blend as Blend
import WebGL.Settings
import Hue

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


type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Time


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

view : Model -> Html Msg
view { texture, theta, particles } =
    Html.div [] [
    Html.p [] [Html.text (toString theta)],
    WebGL.toHtmlWith
        [ 
          WebGL.clearColor 0.0 0.0 0.0 1.0
        ]
        [ width opts.width
        , height opts.height
        , style [ ( "display", "block" ) ]
        ]
        (texture
            |> Maybe.map (scene particles)
            |> Maybe.withDefault []
        )
      ]

scene : List Particle -> Texture -> List Entity
scene particles texture = 
  let 
    partfunc arg = WebGL.entityWith [  
      Blend.add Blend.srcAlpha Blend.one
    ] vertexShader fragmentShader arg { 

      texture=texture,
      resolution=vec2 (toFloat opts.width) (toFloat opts.height)
    }
  in
    List.map (partfunc << particleMesh) particles

triangle : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec4 -> (Vertex, Vertex, Vertex)
triangle a b c d e f col = (
    {
      position = a, 
      texture_coord = d,
      color_attribute = col
    }, 
    {
      position = b, 
      texture_coord = e,
      color_attribute = col
    }, 
    {
      position = c, 
      texture_coord = f,
      color_attribute = col
    }
  )

particleMesh : Particle -> Mesh Vertex
particleMesh particle =
    let 
      size = particle.size/2
      left   = (Vec2.getX particle.position) - size
      right  = (Vec2.getX particle.position) + size
      bottom = (Vec2.getY particle.position) - size
      top    = (Vec2.getY particle.position) + size
    in
    [ 
      triangle
        (vec2 left bottom)
        (vec2 right bottom)
        (vec2 left top)
        (vec2 0.0 0.0)
        (vec2 1.0 0.0)
        (vec2 0.0 1.0)
        particle.color
      ,
      triangle 
        (vec2 left top)
        (vec2 right bottom)
        (vec2 right top)
        (vec2 0.0 1.0)
        (vec2 1.0 0.0)
        (vec2 1.0 1.0)
        particle.color
    ] |> WebGL.triangles

vertexShader : Shader Vertex Uniforms { v_color : Vec4, v_texture_coord : Vec2 }
vertexShader =
    [glsl|
      attribute vec2 position;
      attribute vec2 texture_coord;
      attribute vec4 color_attribute;

      uniform vec2 resolution;
      varying vec4 v_color;
      varying vec2 v_texture_coord;

      void main() {
        vec2 clipSpace = (position/resolution)*2.0-1.0;
        gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
        v_color = color_attribute;
        v_texture_coord = texture_coord;
      }

    |]

type alias Vertex =
    { position : Vec2
    , texture_coord : Vec2
    , color_attribute : Vec4
    }

type alias Uniforms =
    { 
    resolution : Vec2,
    texture : Texture
    }

fragmentShader : Shader {} { u | texture : Texture } { v_color : Vec4, v_texture_coord : Vec2 }
fragmentShader =
    [glsl|
      precision mediump float;
      uniform sampler2D texture;

      varying vec4 v_color;
      varying highp vec2 v_texture_coord;

      void main() {
        vec4 texColor = texture2D(texture, v_texture_coord.xy * vec2(1,-1));

        vec4 finalColor;
        finalColor.r = texColor.r * v_color.r;
        finalColor.g = texColor.g * v_color.g;
        finalColor.b = texColor.b * v_color.b;
        finalColor.a = texColor.a * v_color.a;

        gl_FragColor = finalColor;
      }

    |]
