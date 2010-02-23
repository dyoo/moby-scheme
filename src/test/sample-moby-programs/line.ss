#lang s-exp "../../moby-lang.ss"


(place-image (line 100 0 'black) 50 150 (empty-scene 200 200))
(place-image (line -100 0 'black) 50 150 (empty-scene 200 200))

(check-expect 101 (image-width (line 100 -50 'black)))
(check-expect 101 (image-width (line -100 -50 'black)))
(check-expect 51 (image-height (line -100 -50 'black)))

(image-width (line -100 -50 'black))