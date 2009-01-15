; Example from a posting "Re: DrScheme and XML", 

(require "../../xml.ss"
         "../../html.ss"
         "poetry.ss")

(define 1poem
  (poem title: "Bitter for Sweet"
        poet: "Christina Rossetti"
        tag: "bitter"
        (stanza
         (line "Summer is gone with all its roses,")
         (line "Its sun and perfumes and sweet flowers,")
         (line "Its warm air and refreshing showers:")
         (line "And even Autumn closes."))
        (stanza
         (line "Yea, Autumn's chilly self is going,")
         (line "And winter comes which is yet colder;")
         (line "Each day the hoar-frost waxes bolder")
         (line "And the last buds cease blowing."))))

(define rossetti-collection
  (book title: "Poems of Christina Rossetti"
        
        (poem title: "Bitter for Sweet"
              poet: "Christina Rossetti"
              tag: "bitter"
              (stanza
               (line "Summer is gone with all its roses,")
               (line "Its sun and perfumes and sweet flowers,")
               (line "Its warm air and refreshing showers:")
               (line "And even Autumn closes."))
              (stanza
               (line "Yea, Autumn's chilly self is going,")
               (line "And winter comes which is yet colder;")
               (line "Each day the hoar-frost waxes bolder")
               (line "And the last buds cease blowing.")))
        
        (poem title: "The First Spring Day"
              poet: "Christina Rossetti"
              tag: "spring"
              (stanza
               (line "I wonder if the sap is stirring yet,")
               (line "If wintry birds are dreaming of a mate,")
               (line "If frozen snowdrops feel as yet the sun")
               (line "And crocus fires are kindling one by one:")
               (line "Sing, robin, sing;")
               (line "I still am sore in doubt concerning Spring."))
              
              (stanza
               (line "I wonder if the springtide of this year")
               (line "Will bring another Spring both lost and dear;")
               (line "If heart and spirit will find out their Spring,")
               (line "Or if the world alone will bud and sing:")
               (line "Sing, hope, to me;")
               (line "Sweet notes, my hope, soft notes for memory."))
              
              (stanza
               (line "The sap will surely quicken soon or late,")
               (line "The tardiest bird will twitter to a mate;")
               (line "So Spring must dawn again with warmth and bloom,")
               (line "Or in this world, or in the world to come:")
               (line "Sing, voice of Spring,")
               (line "Till I too blossom and rejoice and sing.")))
        
        (poem title: "On Keats"
              poet: "Christina Rossetti"
              tag: "keats"
              (stanza
               (line "A garden in a garden: a green spot")
               (line "Where all is green: most fitting slumber-place")
               (line "For the strong man grown weary of a race")
               (line "Soon over. Unto him a goodly lot")
               (line "Hath fallen in fertile ground; there thorns are not,")
               (line "But his own daisies: silence, full of grace,")
               (line "Surely hath shed a quiet on his face:")
               (line "His earth is but sweet leaves that fall and rot.")
               (line "What was his record of himself, ere he")
               (line "Went from us ? Here lies one whose name was writ")
               (line "In water: while the chilly shadows flit")
               (line "Of sweet Saint Agnes' Eve; while basil springs,")
               (line "His name, in every humble heart that sings,")
               (line "Shall be a fountain of love, verily.")))
        
        (poem title: "Seasons"
              poet: "Christina Rossetti"
              tag: "seasons"
              (stanza
               (line "In Springtime when the leaves are young,")
               (line "Clear dewdrops gleam like jewels, hung")
               (line "On boughs the fair birds roost among."))
              (stanza
               (line "When Summer comes with sweet unrest,")
               (line "Birds weary of their mothers breast,")
               (line "And look abroad and leave the nest."))
              (stanza
               (line "In Autumn ere the waters freeze,")
               (line "The swallows fly across the seas: -")
               (line "If we could fly away with these!"))
              (stanza
               (line "In Winter when the birds are gone,")
               (line "The sun himself looks starved and wan,")
               (line "And starved the snow he shines upon.")))
        
        (poem title: "Somewhere or Other"
              poet: "Christina Rossetti"
              tag: "other"
              (stanza
               (line "Somewhere or other there must surely be")
               (line "The face not seen, the voice not heard,")
               (line "The heart that not yet-never yet-ah me!")
               (line "Made answer to my word."))
              (stanza
               (line "Somewhere or other, may be near or far;")
               (line "Past land and sea, clean out of sight;")
               (line "Beyond the wandering moon, beyond the star")
               (line "That tracks her night by night."))
              (stanza
               (line "Somewhere or other, may be far or near;")
               (line "With just a wall, a hedge, between;")
               (line "With just the last leaves of the dying year")
               (line "Fallen on a turf grown green.")))
        
        (poem title: "The Wind"
              poet: "Christina Rossetti"
              tag: "wind"
              (stanza
               (line "The wind has such a rainy sound")
               (line "Moaning through the town,")
               (line "The sea has such a windy sound,-")
               (line "Will the ships go down?"))
              (stanza
               (line "The apples in the orchard")
               (line "Tumble from their tree.-")
               (line "Oh will the ships go down, go down,")
               (line "In the windy sea?")))))

(define poetry-stylesheet
  (stylesheet
    
    (define-element toc)
    
    (xml-macro poem
      (lambda (x)
        (xml-match x
          ((poem title: ,t poet: ,a tag: ,m
                 (stanza (line ,l1) (line ,l) ...) ...)
           (h4:div (h4:p) (h4:a h4:name: m) (h4:strong t) (h4:br) (h4:em a)
                   (list (h4:p) l1 (list (h4:br) l) ...) ...)))))
    
    (xml-macro toc
      (lambda (x)
        (xml-match x
          ((toc (poem title: ,t poet: ,a tag: ,m . ,rest) ...)
           (list (h4:p) "Table of Contents:"
                 (h4:ul (h4:li (h4:a h4:href: (string-append "#" m) t)) ...))))))
    
    (xml-micro book
      (lambda (x)
        (xml-match x
          ((book title: ,bt ,p ...)
           (h4:html (h4:head (h4:title bt))
                    (h4:body (h4:h1 bt)
                             (xml-expand (toc p ...))
                             (xml-expand (list p ...))))))))
    
    ))

(define poetry->html
  (stylesheet->expander poetry-stylesheet))

(with-output-to-file "cr.html"
  (lambda () (write-xml (poetry->html rossetti-collection)))
  'replace)


