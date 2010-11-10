#lang scribble/manual

@(require unstable/scribble
          (for-label (only-in racket/base planet))
          (for-label (planet dyoo/moby:3))

	  (for-label (planet dyoo/moby:3/phone/location))
          (for-label (planet dyoo/moby:3/phone/tilt))
          (for-label (planet dyoo/moby:3/phone/sms)))

@title{Moby: the Moby Scheme Compiler}

@section{What is Moby?}

Moby is a project from the @hyperlink["http://racket-lang.org/people.html"]{PLT} team.  The Moby compiler
consumes Advanced Student Language (ASL) programs that use @link["http://world.cs.brown.edu/"]{World}
primitives, and produces applications for mobile platforms.  The
current prototype supports desktop browsers and smartphones.
Our long-term goal is to make Racket the premiere reactive scripting
language for mobile phones.

Shriram Krishnamurthi presented the ideas behind Moby at ILC 2009 in
his talk @link["http://www.cs.brown.edu/~sk/Publications/Talks/Moby-Bootstrap/"]{The Moby Scheme Compiler for Smartphones}.

Moby requires Racket 5.0.1.



@section{Quick Start}


Let's see if Moby has been installed on your system.  Run the following simple program.
@racketmod[planet #,(this-package-version-symbol)
"hello world"
true
(define (f x) (* x x))

(check-expect (f 42) 1764)

(check-expect (map f '(1 2 3 4 5))
              (list 1 4 9 16 25))
]

On the very first run of this program, Racket may pause as it installs
the Moby PLaneT package and generates its documentation.  Expect to wait a
few minutes for the installation to complete.


Moby programs can run in DrRacket as long as they don't use any
Javascript-specific functions.  Let's create a simple application that
does use a Javascript context.  The following example shows a rapidly
incrementing counter.  Create a file @filepath{counter.rkt} in the
Module language with this content:

@racketmod[planet #,(this-package-version-symbol)
(define initial-world 0)
(big-bang initial-world (on-tick add1))
]


Note that this program is in a separate language that provides extra
functions like @racket[big-bang].  This program can be executed in
Racket, but evaluation will halt on the @racket[big-bang] because
it's a function that requires a Javascript web context.


For testing, the function @racket[run-in-browser] can be used to provide a mock
environment in your web browser:
@racketmod[racket
(require (planet #,(this-package-version-symbol)))
(run-in-browser "counter.rkt")
]
This will bring up a web server and a browser window with the running program.


To create an Android apk package, you can use @racket[create-android-phone-package].
Create a file called @filepath{build-counter.rkt} with the following content:
@racketmod[racket
(require (planet #,(this-package-version-symbol)))
(create-android-phone-package "counter.rkt" "counter.apk")                  
]
Running this will take @filepath{counter.rkt} and compile it to an Android package
that can be installed.

(As a warning, package generation may take about 30 seconds to complete.)








Because Moby programs use the web, they can dynamically
generate DOM trees and style them with CSS, as in the examples below.

The next example renders the world as a paragraph of text, styled
with a font-size of 30.  It uses @racket[draw-page] and @racket[draw-css]
to draw the web page.

@racketmod[planet #,(this-package-version-symbol)
(define initial-world 0)
  
(define (draw-html w)
  (list (js-p '(("id" "myPara")))
        (list (js-text "hello world"))))

(define (draw-css w)
  '(("myPara" ("font-size" "30"))))
  
(big-bang initial-world
          (to-draw-page draw-html draw-css))]




The next example shows an image and an input text field.  As with the
previous example, it uses @racket[draw-html] and @racket[draw-css] to
construct the web page, and every time the world changes, the runtime environment
reacts by re-drawing the web page.

@racketmod[planet #,(this-package-version-symbol)

(define (form-value w)
  (format "~a" w))

(define (update-form-value w v)
  (string->number v))

(define elt
  (js-input "text" update-form-value '()))

(define (draw-html w)
  (list (js-div)
        (list (js-img "http://plt-scheme.org/logo.png"))
        (list elt)
        (list (js-p '(("id" "aPara")))
              (list (js-text (format "~a" w))))))

(define (draw-css w)
  '(("aPara" ("font-size" "50px"))))

(big-bang false
             (to-draw-page draw-html draw-css))]



We can also use phone-specific features, such as geolocation.
The following program shows the current location.
@racketmod[planet #,(this-package-version-symbol)
(require #,(schememodname/this-package phone/location))

(define (make-message w lat lng)
  (format "I think I am at: ~s ~s" lat lng))

(big-bang "initial state"
	  (on-location-change make-message))]
Note that it requires @racket[phone/location], one of the
modules provided by this package.





The last example is a phone mood ring called @filepath{mood-ring.rkt}:
it shows a single DIV whose background color is controlled by the
phone's orientation; it uses @racket[phone/tilt] to get at the
orientation of the phone, and arbitrarily maps it to a color.

@racketmod[planet #,(this-package-version-symbol)
(require #,(schememodname/this-package phone/tilt))

@code:comment{The world is a color.}
(define initial-world (make-color 0 0 0))

@code:comment{tilt: world number number number -> world}
@code:comment{Tilting the phone adjusts the color.}
(define (tilt w azimuth pitch roll)
  (make-color (scale azimuth 360)
	      (scale (+ pitch 90) 180)
	      (scale (+ roll 90) 180)))

@code:comment{scale-azimuth: number -> number}
@code:comment{Take a number going from 0-360 and scale it to a number between 0-255}
(define (scale n domain-bound)
  (inexact->exact (floor (* (/ n domain-bound) 255))))

@code:comment{User interface.}
(define view (list (js-div '((id "background")))))

(define (draw-html w) view)

(define (draw-css w)
  (list (list "background" 
	      (list "background-color" 
		    (format "rgb(~a, ~a, ~a)"
			    (color-red w)
			    (color-green w)
			    (color-blue w)))
	      (list "width" "300")
	      (list "height" "300"))))



(big-bang initial-world
	  (on-tilt tilt)
	  (to-draw-page draw-html draw-css))

]
Again, to package the program, we use @racket[create-android-phone-package].
@racketmod[racket
(require #,(schememodname/this-package))
(create-android-phone-package "mood-ring.rkt" "mood.apk")
]







@section{Running and packaging Android programs}

@defmodule/this-package[]

                                                                       
@defproc[(run-in-browser [input-file path-string?]) void]{
Runs the given @racket[input-file] in a context that provides mocks for
phone-specific behavior.}


@defproc[(create-android-phone-package [input-file path-string?]
                                       [output-apk path-string?]) void]{
Creates an Android phone package.}

            
 





@section{Phone API}

@subsection{Location (GPS)}
@defmodule/this-package[phone/location]

@defproc[(on-location-change [world-updater (world [latitude number] [longitude number] -> world)]) handler]{Constructs a world handler that watches for changes in the phone's geographic location.
}



@;;; Commenting out sms messaging temporarily
@;{
@subsection{SMS Messaging}
@defmodule/this-package[phone/sms]
@defproc[(on-sms-receive [world-updater (world [sender string] [message string] -> world)]) handler]{
Constructs a world handler that watches for incoming SMS messages.}
}



@subsection{Motion sensors and tilt}
@defmodule/this-package[phone/tilt]

@defproc[(on-acceleration  [world-updater (world [x number] [y number] [z number] -> world)]) handler]{
Constructs a world handler that watches acceleration updates.}

@defproc[(on-shake [world-updater (world -> world)]) handler]{
Constructs a world handler that watches shake events; if the phone is
shaken, the world-updater will fire off.}


@defproc[(on-tilt [world-updater (world [azimuth number] [pitch number] [roll number] -> world)]) handler]{Constructs a world handler that watches changes in orientation.}






                                                                       

@section{API}

  
The language bindings of Moby language come from the  @hyperlink["http://planet.racket-lang.org/display.ss?package=js-vm.plt&owner=dyoo"]{js-vm}
PLaneT package; please refer to the documentation of @emph{js-vm}.

