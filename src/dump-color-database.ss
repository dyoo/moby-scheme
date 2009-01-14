#lang scheme/base
(require scheme/gui/base
         scheme/class
         scheme/list)

(define (string-strip s)
  (regexp-replace* #px"\\s+$" 
                   (regexp-replace* #px"^\\s+" s "") ""))

(define names 
  (map string-strip
  
  (regexp-split "\n" 
                            #<<EOF
        Orange  
        Red
        OrangeRed
        Tomato
        DarkRed
        Red
        Firebrick
        Crimson
        DeepPink
        Maroon
        Indian Red
        IndianRed
        Medium Violet Red
        MediumVioletRed
        Violet Red
        VioletRed
        LightCoral
        HotPink
        PaleVioletRed
        LightPink
        RosyBrown
        Pink
        Orchid
        LavenderBlush
        Snow
        Chocolate
        SaddleBrown
        Brown
        DarkOrange
        Coral
        Sienna
        Orange
        Salmon
        Peru
        DarkGoldenrod
        Goldenrod
        SandyBrown
        LightSalmon
        DarkSalmon
        Gold
        Yellow
        Olive
        Burlywood
        Tan
        NavajoWhite
        PeachPuff
        Khaki
        DarkKhaki
        Moccasin
        Wheat
        Bisque
        PaleGoldenrod
        BlanchedAlmond
        Medium Goldenrod
        MediumGoldenrod
        PapayaWhip
        MistyRose
        LemonChiffon
        AntiqueWhite
        Cornsilk
        LightGoldenrodYellow
        OldLace
        Linen
        LightYellow
        SeaShell
        Beige
        FloralWhite
        Ivory
        Green
        LawnGreen
        Chartreuse
        Green Yellow
        GreenYellow
        Yellow Green
        YellowGreen
        Medium Forest Green
        OliveDrab
        MediumForestGreen
        Dark Olive Green
        DarkOliveGreen
        DarkSeaGreen
        Lime
        Dark Green
        DarkGreen
        Lime Green
        LimeGreen
        Forest Green
        ForestGreen
        Spring Green
        SpringGreen
        Medium Spring Green
        MediumSpringGreen
        Sea Green
        SeaGreen
        Medium Sea Green
        MediumSeaGreen
        Aquamarine
        LightGreen
        Pale Green
        PaleGreen
        Medium Aquamarine
        MediumAquamarine
        Turquoise
        LightSeaGreen
        Medium Turquoise
        MediumTurquoise
        Honeydew
        MintCream
        RoyalBlue
        DodgerBlue
        DeepSkyBlue
        CornflowerBlue
        Steel Blue
        SteelBlue
        LightSkyBlue
        Dark Turquoise
        DarkTurquoise
        Cyan
        Aqua
        DarkCyan
        Teal
        Sky Blue
        SkyBlue
        Cadet Blue
        CadetBlue
        Dark Slate Gray
        DarkSlateGray
        LightSlateGray
        SlateGray
        Light Steel Blue
        LightSteelBlue
        Light Blue
        LightBlue
        PowderBlue
        PaleTurquoise
        LightCyan
        AliceBlue
        Azure
        Medium Blue
        MediumBlue
        DarkBlue
        Midnight Blue
        MidnightBlue
        Navy
        Blue
        Indigo
        Blue Violet
        BlueViolet
        Medium Slate Blue
        MediumSlateBlue
        Slate Blue
        SlateBlue
        Purple
        Dark Slate Blue
        DarkSlateBlue
        DarkViolet
        Dark Orchid
        DarkOrchid
        MediumPurple
        Cornflower Blue
        Medium Orchid
        MediumOrchid
        Magenta
        Fuchsia
        DarkMagenta
        Violet
        Plum
        Lavender
        Thistle
        GhostWhite
        White
        WhiteSmoke
        Gainsboro
        Light Gray
        LightGray
        Silver
        Gray
        Dark Gray
        DarkGray
        Dim Gray
        DimGray
        Black
EOF
  )))


;; color-tuples: (listof (list string number number number)))
(define color-tuples
  (for/list ([name names])
    (let ([color
           (send the-color-database find-color name)])
      (list name
            (send color red)
            (send color green)
            (send color blue)))))



(define (generate-code)
  (define (format-tuple a-tuple)
    (format "db.put(~s, new Color(~a, ~a, ~a));\n"
            (string-upcase (first a-tuple))
            (second a-tuple)
            (third a-tuple)
            (fourth a-tuple)))  
  (string-append "static Hashtable db = new Hashtable();\n"
                 "static { \n"
                 (apply string-append (for/list ([a-tuple color-tuples])
                                        (format-tuple a-tuple)))
                 "}"))
