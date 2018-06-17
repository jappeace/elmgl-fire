module View exposing (..)
{-
Stuff we view
 -}

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Error, Texture)
import WebGL.Settings.Blend as Blend
import Model exposing (Model, opts, Msg, Particle, Vertex, Uniforms)

view : Model -> Html Msg
view { time, particles } =
    Html.div [] [
    Html.p [] [Html.text (toString time)],
    WebGL.toHtmlWith
        [ 
          WebGL.clearColor 0.0 0.0 0.0 1.0
        ]
        [ width opts.width
        , height opts.height
        , style [ ( "display", "block" ) ]
        ]
        (particles)
      ]

scene : List Particle -> Uniforms -> List Entity
scene particles uni = 
  let 
    partfunc arg = WebGL.entityWith [  
      Blend.add Blend.srcAlpha Blend.one
    ] vertexShader fragmentShader arg uni
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
