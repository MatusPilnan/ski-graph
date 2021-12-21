module Icons exposing (..)

import Svg exposing ( svg, path )
import Svg.Attributes as SvgAttr


mapIcon =
  svg
    [ SvgAttr.class "h-6 w-6"
    , SvgAttr.fill "none"
    , SvgAttr.viewBox "0 0 24 24"
    , SvgAttr.stroke "currentColor"
    ]
    [ path
      [ SvgAttr.strokeLinecap "round"
      , SvgAttr.strokeLinejoin "round"
      , SvgAttr.strokeWidth "2"
      , SvgAttr.d "M9 20l-5.447-2.724A1 1 0 013 16.382V5.618a1 1 0 011.447-.894L9 7m0 13l6-3m-6 3V7m6 10l4.553 2.276A1 1 0 0021 18.382V7.618a1 1 0 00-.553-.894L15 4m0 13V4m0 0L9 7"
      ]
      []
    ]
