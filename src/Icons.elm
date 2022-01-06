module Icons exposing (..)

import Html
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


errorIcon =
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
      , SvgAttr.d "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
      ]
      []
    ]


liftIcon : Html.Html msg
liftIcon =
  svg
    [ SvgAttr.viewBox "0 0 512.000000 512.000000"
    , SvgAttr.preserveAspectRatio "xMidYMid meet"
    ]
    [ Svg.g
      [ SvgAttr.transform "translate(0.000000,512.000000) scale(0.100000,-0.100000)"
      , SvgAttr.fill "#000000"
      , SvgAttr.stroke "none"
      ]
      [ path
        [ SvgAttr.d "M3385 5108 c-16 -6 -644 -247 -1395 -534 -1502 -576 -1414 -538 -1440 -635 -20 -73 20 -158 91 -194 66 -35 96 -26 836 262 10 4 13 -282 16 -1334 3 -1500 -3 -1386 78 -1549 96 -195 270 -336 488 -396 70 -20 102 -22 436 -22 401 -1 397 -2 450 68 49 64 46 152 -8 213 -54 62 -62 63 -411 63 -195 0 -335 4 -368 11 -123 26 -218 101 -275 217 l-38 76 -3 1396 -2 1396 842 323 c464 178 855 332 870 342 32 23 68 94 68 135 -1 45 -34 109 -74 139 -42 32 -112 42 -161 23z"
        ]
        []
      , path
        [ SvgAttr.d "M2295 4057 c-110 -37 -190 -104 -239 -200 -75 -150 -38 -339 89 -456 225 -205 580 -79 637 228 30 158 -63 331 -216 406 -49 24 -75 30 -145 32 -52 2 -102 -2 -126 -10z"
        ]
        []
      , path
        [ SvgAttr.d "M2241 3034 c-102 -27 -172 -91 -213 -193 -23 -56 -23 -60 -26 -681 -2 -424 0 -645 8 -688 17 -100 65 -163 155 -203 48 -21 64 -22 472 -27 l422 -5 195 -293 c108 -162 196 -297 196 -301 0 -5 -103 -75 -227 -158 -248 -164 -286 -193 -309 -237 -42 -81 -3 -191 80 -229 52 -23 90 -23 141 0 43 19 1314 869 1352 903 43 40 51 76 72 340 20 237 20 261 6 295 -52 124 -204 153 -287 55 -38 -45 -45 -78 -58 -262 -6 -91 -13 -175 -16 -188 -3 -15 -51 -53 -161 -127 l-157 -104 -241 362 c-133 199 -258 377 -279 395 -72 63 -83 65 -388 70 l-278 4 0 95 0 95 28 -8 c54 -17 111 -5 352 70 368 115 374 118 405 201 43 111 -37 225 -158 225 -28 0 -125 -25 -252 -65 -113 -36 -208 -65 -209 -65 -2 0 -40 56 -85 125 l-81 124 0 98 c0 137 -21 200 -88 275 -94 104 -228 141 -371 102z"
        ]
        []
      ]
    ]


skiRunIcon : String -> Html.Html msg
skiRunIcon color =
  svg
    [ SvgAttr.viewBox "0 0 512.000000 512.000000"
    , SvgAttr.preserveAspectRatio "xMidYMid meet"
    ]
    [ Svg.g
      [ SvgAttr.transform "translate(0.000000,512.000000) scale(0.100000,-0.100000)"
      , SvgAttr.fill color
      , SvgAttr.stroke "none"
      ]
      [ path
        [ SvgAttr.d "M4311 5094 c-239 -64 -387 -313 -326 -547 30 -114 85 -194 180 -262 87 -62 141 -78 260 -78 115 0 176 18 266 81 133 93 205 270 179 440 -40 257 -309 433 -559 366z"
        ]
        []
      , path
        [ SvgAttr.d "M1497 4790 c-38 -50 -72 -90 -76 -90 -4 0 -52 18 -105 41 -54 22  -100 39 -102 37 -2 -2 28 -43 66 -91 39 -47 70 -92 70 -100 0 -7 -31 -56 -70 -107 -38 -51 -70 -95 -70 -97 0 -2 19 -13 43 -23 73 -34 175 -21 247 30 15 11 35 20 43 20 8 0 116 -73 239 -162 304 -219 863 -629 1031 -755 l137 -103 156 65 c86 36 157 65 159 65 1 0 5 -93 7 -207 5 -233 11 -259 76 -327 81 -85 270 -188 622 -339 229 -98 321 -129 400 -135 60 -4 68 -2 98 23 39 32 42 69 11 131 -41 85 -193 205 -601 473 -99 65 -147 102 -147 114 -1 11 19 86 44 167 92 302 129 494 130 665 0 109 -3 125 -26 175 -33 72 -76 120 -150 169 -126 84 -239 105 -481 90 -357 -22 -664 -127 -908 -311 -59 -44 -95 -66 -104 -61 -33 20 -455 333 -513 381 l-65 54 7 72 c4 51 2 84 -8 112 -15 46 -64 114 -81 114 -6 0 -42 -41 -79 -90z"
        ]
        []
      , path
        [ SvgAttr.d "M1902 3683 c-73 -105 -104 -210 -104 -362 0 -140 22 -216 95 -334 87 -138 134 -180 444 -388 159 -106 308 -211 331 -233 103 -95 110 -188 23 -306 -81 -110 -387 -493 -397 -497 -11 -4 -876 485 -1746 988 l-257 149 -71 2 c-63 1 -75 -1 -113 -28 -84 -59 -116 -188 -61 -253 32 -38 161 -124 389 -258 275 -163 2285 -1315 2920 -1675 878 -497 818 -471 1100 -472 159 -1 188 2 250 21 145 46 324 183 374 286 47 96 9 212 -84 257 -81 39 -174 20 -240 -50 -105 -111 -235 -150 -394 -120 -106 20 -229 85 -951 500 -560 322 -588 338 -594 348 -6 10 52 84 194 252 376 440 456 559 487 718 15 75 15 89 0 164 -25 125 -66 198 -171 304 -107 107 -204 181 -706 535 -651 460 -678 479 -688 479 -6 0 -19 -12 -30 -27z"
        ]
        []
      ]
    ]


saveIcon =
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
      , SvgAttr.d "M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"
      ]
      []
    ]

menu =
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
    , SvgAttr.d "M4 6h16M4 12h16M4 18h16"
    ]
    []
  ]

edit =
  svg
    [ SvgAttr.class "h-5 w-5"
    , SvgAttr.viewBox "0 0 20 20"
    , SvgAttr.fill "currentColor"
    ]
    [ path
        [ SvgAttr.d "M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z"
        ]
        []
    ]

remove =
    svg
    [ SvgAttr.class "h-5 w-5"
    , SvgAttr.viewBox "0 0 20 20"
    , SvgAttr.fill "currentColor"
    ]
    [ path
        [ SvgAttr.fillRule "evenodd"
        , SvgAttr.d "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z"
        , SvgAttr.clipRule "evenodd"
        ]
        []
    ]

detail =
  svg
  [ SvgAttr.class "h-5 w-5"
  , SvgAttr.viewBox "0 0 20 20"
  , SvgAttr.fill "currentColor"
  ]
  [ path
    [ SvgAttr.d "M10 6a2 2 0 110-4 2 2 0 010 4zM10 12a2 2 0 110-4 2 2 0 010 4zM10 18a2 2 0 110-4 2 2 0 010 4z"
    ]
    []
  ]


highlight =
  svg
  [ SvgAttr.class "h-5 w-5"
  , SvgAttr.viewBox "0 0 20 20"
  , SvgAttr.fill "currentColor"
  ]
  [ path
    [ SvgAttr.d "M10 12a2 2 0 100-4 2 2 0 000 4z"
    ]
    []
  , path
    [ SvgAttr.fillRule "evenodd"
    , SvgAttr.d "M.458 10C1.732 5.943 5.522 3 10 3s8.268 2.943 9.542 7c-1.274 4.057-5.064 7-9.542 7S1.732 14.057.458 10zM14 10a4 4 0 11-8 0 4 4 0 018 0z"
    , SvgAttr.clipRule "evenodd"
    ]
    []
  ]


chevronRight =
  svg
  [ SvgAttr.class "h-5 w-5"
  , SvgAttr.viewBox "0 0 20 20"
  , SvgAttr.fill "currentColor"
  ]
  [ path
    [ SvgAttr.fillRule "evenodd"
    , SvgAttr.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
    , SvgAttr.clipRule "evenodd"
    ]
    []
  ]

chevronLeft =
  svg
  [ SvgAttr.class "h-5 w-5"
  , SvgAttr.viewBox "0 0 20 20"
  , SvgAttr.fill "currentColor"
  ]
  [ path
    [ SvgAttr.fillRule "evenodd"
    , SvgAttr.d "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
    , SvgAttr.clipRule "evenodd"
    ]
    []
  ]
