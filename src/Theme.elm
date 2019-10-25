module Theme exposing (backgroundColor, hoverColor, smallShadow, textColor)

import Css exposing (boxShadow5, px, rgb, rgba, zero)


textColor =
    rgb 60 50 50


backgroundColor =
    rgb 255 255 255


hoverColor =
    rgb 100 100 255


smallShadow =
    boxShadow5 zero (px 1) (px 3) zero (rgba 0 0 0 0.1)
