#lang scribble/manual


@(require (for-syntax scheme/base))
@(define-syntax (mobyblock stx)
   (syntax-case stx ()
     [(header body ...)
      (let* ([source-name (syntax-source #'header)]
             [line (syntax-line #'header)]
             [column 0]
             [position (syntax-position #'header)]
             [planet-symbol 'planet]
             [moby-symbol 'dyoo/moby:1]
             [planet-symbol-length (string-length (symbol->string planet-symbol))]
             [moby-symbol-length (string-length (symbol->string moby-symbol))])
        (with-syntax ([planet-mod (datum->syntax #f 'planet (list source-name
                                                                  line 
                                                                  column 
                                                                  position 
                                                                  (string-length
                                                                   (symbol->string planet-symbol))))]
                      [lang (datum->syntax #f 'dyoo/moby:1 (list source-name 
                                                                 line 
                                                                 (+ column planet-symbol-length 1)
                                                                 (+ position planet-symbol-length 1) 
                                                                 moby-symbol-length))])
          (syntax/loc stx
            (schememod planet-mod lang
                       body ...))))]))


@(require (for-label "src/moby-lang.ss"))

@title{Moby: the Moby Scheme Compiler}

@section{What is Moby?}

Moby is a project from the PLT Scheme team.  The Moby compiler
consumes Advanced Student Language (ASL) programs that use World
primitives, and produces applications for mobile platforms.  The
current prototype supports desktop browsers and smartphones.
Our long-term goal is to make Scheme the premiere reactive scripting
language for mobile phones.

Shriram Krishnamurthi presented the ideas behind Moby at ILC 2009 in
his talk @link["http://www.cs.brown.edu/~sk/Publications/Talks/Moby-Bootstrap/"]{The Moby Scheme Compiler for Smartphones}.

    


    
@section{Running Moby from DrScheme}


To use Moby from DrScheme, create a file in the Module language, and
at the top of your program, include the following language line:

@mobyblock[
]


followed by the program.  For example, running the program:

@mobyblock[
(define initial-world 0)
(js-big-bang initial-world (on-tick 1 add1))
]

will invoke a web browser, which should show the
running program on a web page.  Because @scheme[on-tick] is used, 
as every second passes, a tick stimulus is sent to the program.

The page should also provide links to download packages
of the compiled program.



Because these programs run on the user's web browser, we also allow programs to dynamically
generate DOM trees and style them with CSS, as in the examples below.

The following will render the world as a paragraph of text, styled
with a font-size of 30.  @scheme[draw-html] and @scheme[draw-css]
will be called to draw the web page.

@(mobyblock
(define initial-world 0)
  
(define (draw-html w)
  (list (js-p '(("id" "myPara")))
        (list (js-text "hello world"))))

(define (draw-css w)
  '(("myPara" ("font-size" "30"))))

  
(js-big-bang initial-world
             (on-draw draw-html draw-css)))


The following will show a logo and an input text field.  As with the
previous example, @scheme[draw-html] and @scheme[draw-css] are used to
construct the web page; every time the world changes, these functions
are called to re-draw the web page.

@mobyblock[
(define (form-value w)
  (format "~a" w))

(define (update-form-value w v)
  (string->number v))

(define elt
  (js-input "text" update-form-value))

(define (draw-html w)
  (list (js-div)
        (list (js-img "http://plt-scheme.org/logo.png"))
        (list elt)
        (list (js-p '(("id" "aPara")))
              (list (js-text (format "~a" w))))))

(define (draw-css w)
  '(("aPara" ("font-size" "50px"))))

(js-big-bang 0
             (on-draw draw-html draw-css))]





@section{The Moby World API}
@declare-exporting[(planet dyoo/moby:1/src/moby-lang)]
  

@defproc[(js-big-bang (a-world world) (handlers handler?) ...) void]{
A Moby program starts a reactive computation with @scheme[js-big-bang].
The rest of the arguments hook into the reactive computation.  One
instance of a handler is @scheme[on-tick], which registers a function to update
the world on a clock tick.

In the absence of an @scheme[on-draw] or @scheme[on-redraw] handler, @scheme[js-big-bang] will
show the current state of the world; on transitions, the world is
re-drawn to screen.}



@defproc[(on-draw [to-dom (world -> (DOM-sexp))]
                  [to-css (world -> (CSS-sexp))]) scene]{
One of the main handlers to @scheme[js-big-bang] is @scheme[on-draw], which controls how
the world is rendered on screen.  The first argument computes a
rendering of the world as a DOM tree, and the second argument computes
that tree's styling.
}


@defproc[(stop-when [stop? (world -> boolean)]) handler?]{
When the world should be stopped --- when @scheme[stop?] applied to the world
produces @scheme[true] --- then the @scheme[js-big-bang] terminates.
}
                                                          
                                                        
@defproc[(on-redraw [hook (world -> scene)]) handler?]{
For simple applications, on-redraw is sufficient to draw a scene onto the display.

The following program shows a ball falling down a scene.

@(mobyblock
(define WIDTH 320)
(define HEIGHT 480)
(define RADIUS 15)

(define INITIAL-WORLD 0)

(define (tick w)
  (+ w 5))

(define (hits-floor? w)
  (>= w HEIGHT))

(check-expect (hits-floor? 0) false)
(check-expect (hits-floor? HEIGHT) true)

(define (render w)
  (place-image (circle RADIUS "solid" "red") (/ WIDTH 2) w
               (empty-scene WIDTH HEIGHT)))

(js-big-bang INITIAL-WORLD
             (on-tick 1/15 tick)
             (on-redraw render)
             (stop-when hits-floor?)))
}



@subsection{Types}

A @scheme[dom-sexp] describes the structure of a web page:

@schemegrammar[dom-sexp (list dom-element dom-sexp ...)]


a @scheme[css-sexp] describes the structure of a page's styling:

@schemegrammar[css-sexp (listof (cons (or dom-element string)
                                      (listof attrib)))]

An @scheme[attrib] is a:
@schemegrammar[attrib (list string string)]

Each of the @scheme[dom-element]s can take in an optional attribute list to
assign to the new dom element; the common useful attribute is a key-value binding for an "id",
which can be used to identify an element in the css-drawing function.


Here are examples  of a dom-expr and a css-sexp.
@schemeblock[
(define a-dom-sexp (list (js-div '(("id" "main-div")))
                         (list (js-text "Hello world"))))

(define a-css-sexp (list (list "main-div"
                               (list "background" "white")
                               (list "font-size" "40px"))))
             ]


@subsection{@scheme[dom-element] constructors}

Here are the dom-element constructors.

@defproc[(js-div (attribs (listof attrib?) '())) dom-element?]{
Constructs a div element.}


@defproc[(js-p (attribs (listof attrib?) '())) dom-element?]{
Constructs a paragraph element.}

@defproc[(js-button (world-update-f (world -> world)) 
                    (attribs (listof attrib) '()))
         dom-element]{
Constructs a button.  When the button is pressed, the world is updated through @scheme[world-update-f].
}
                     
                     

@defproc[(js-button* (world-update-f (world -> world))
                     (effect-f (world -> effect))
                     (attribs (listof attrib) '()))
         dom-element]{
Constructs a button.  When the button is pressed, the original world is updated,
and the original world is used to construct an effect.
}

@defproc[(js-text (text string?)) dom-element]{Constructs regular text.}

@defproc[(js-input (type string) (world-update-f (world string -> world)) (attribs (listof attrib) '())) dom-element]{
Creates an input form element.  When the user changes the content of the form element,
the runtime uses @scheme[world-update-f] to update the world with the string value of the element.

The example below has asingle text input form element, which allows the user to enter
some value.  That value is read from the interface by the @scheme[refresh] function that's associated
to the button.

@(mobyblock
(define input-node
  (js-input "text" '(("id" "myname"))))

(define (refresh w)
  (get-input-value "myname"))

(define (draw w)
  (list (js-div)
        (list (js-div) (list (js-text (format "I see: ~s~n" w))))
        (list (js-div) (list input-node))
        (list (js-div) (list (js-button refresh) (list (js-text "Update!"))))))

(define (draw-css w)
  '())

(js-big-bang "" 
             (on-draw draw draw-css)))
}


@defproc[(js-img (url string) (attribs (listof attrib) '())) dom-element]{Creates an image element.
}



    
             
             
@subsection{Stimulus Handlers}

Stimulus handlers are provided as additional arguments to a js-big-bang.

Each stimulus has an unstarred and a starred version; the starred
version allows you to provide an effect-generating function.  When
the given stimulus emits, the old world is used to compute both
the new world and the optional effect.  Afterwards, each effect in the
effect group is applied.

@schemegrammar[effect atomic-effect
                      (listof effect)]


@deftogether[(
              @defproc[(on-tick (delay number)
                               (world-update-f (world -> world)))
               handler]
               @defproc[(on-tick* (delay number) 
                                  (world-update-f (world -> world))
                                  (effect-f (world -> effect))) 
                        handler]
               )]{
Delays by n seconds, and then calls the handlers.}


@deftogether[(
              @defproc[(on-shake (world-update-f (world -> world))) handler]
               @defproc[(on-shake* (world-update-f (world -> world))
                                   (effect-f (world -> effect)))
                        handler])]{
Calls the shake handlers when the phone is physically jerked.}


@deftogether[(
              @defproc[(on-location-change (world-update-f (world (lat number) (long number) -> world)))
                       handler]
               @defproc[(on-location-change* (world-update-f (world (lat number) (long number) -> world))
                                             (effect-f (world number number -> effect))) handler])]{
Calls the location handlers when the latitude or longitude of the
device has changed.}


@deftogether[(
              @defproc[(on-tilt (world-update-f (world number number number -> world))) handler]
               @defproc[(on-tilt* (world-update-f (world number number number -> world))
                                  (effect-f (world number number number -> effect))) handler])]{
Calls the tile handlers when the phone has been tilted.
}

@deftogether[(
              @defproc[(on-acceleration (world-update-f (world number number number -> world)))
                       handler]
               @defproc[(on-acceleration* (world-update-f (world number number number -> world))
                                          (effect-f (world number number number -> effect)))
                        handler])]{
Calls the acceleration handlers when the device feels change in acceleration.
}



@subsection{Effects}

Effects allow world programs to apply side effects to the outside
world.  These are used in conjunction with the starred version of the
stimulus handlers described above.


@defproc[(make-effect:none) effect]{No result when interpreted.}
@defproc[(make-effect:beep) effect]{Audible beep when interpreted.}


@defproc[(make-effect:play-sound (a-sound sound)) effect]{Plays a sound from the given @scheme[sound].  If the sound is already playing, then the sound continues to play.}
@defproc[(make-effect:stop-sound (a-sound sound)) effect]{Stops playing a sound from the given url.}
@defproc[(make-effect:pause-sound (a-sound sound)) effect]{Pauses a sound; if @scheme[make-effect:play-sound] for the same sound is given later, play restarts from the point where it was paused.}

A @scheme[sound] is a:
@schemegrammar[sound string
                     playlist]


@defproc[(make-effect:set-sound-volume (volume number)) effect]{Sets the sound volume; the number should be between 0 and 100.}
@defproc[(make-effect:raise-sound-volume) effect]{Raises the sound volume.  If the volume's already at 100, has no effect.}
@defproc[(make-effect:lower-sound-volume) effect]{Lowers the sound volume.  If the volume's set to 0, has no effect.}

@defproc[(make-effect:play-dtmf-tone (tone number)) effect]{On a smartphone, plays a DTMF tone, where @scheme[tone] is between 0 and 15 inclusive.}

@defproc[(make-effect:set-wake-lock (flag number)) effect]{On a smartphone, sets the wake-lock flag to prevent the phone from sleeping.  Low-level call.}
@defproc[(make-effect:release-wake-lock) effect]{On a smartphone, releases a wake-lock to allow the phone to go to sleep again.}

@defproc[(make-effect:send-sms (phone-number string) (message string)) effect]{Sends an SMS message.}

@; Commenting out the generate-random documentation
@;{@defproc[(make-effect:generate-random: (world-update-f (world number -> world))) effect]{When interpreted, generates a random number on the fly and uses @scheme[world-update-f] to update the world.}}

@defproc[(make-effect:pick-playlist (world-update-f (world playlist -> world))) effect]{Brings up a playlist picker; when a playlist is selected, the world is updated using @scheme[world-update-f] with the selected @scheme[playlist] sound.}








@subsection{API Extensions}


The following helper functions and forms are provided by Moby.

@defproc[(get-url [url string?]) string?]{

Does an HTTP GET to download the string content from a URL.

As an example, if @scheme[get-url] is called on the URL to the PLT Scheme homepage,
@schemeblock[
(get-url "http://plt-scheme.org/")
]
it produces the textual content of the @filepath{index.html} on the homepage.
}

@defproc[(location-distance (lat-1 number?) (long-1 number?) (lat-2 number?) (long-2 number?)) number?]{
Given latitude-1, longitude-1, latitude-2, longitude-2, produces the
distance between the locations in terms of meters.
}

@defproc[(xml->s-exp (xml-string string?)) s-expression?]{
Parses a string containing XML to s-expressions.
}



@defform[(define-struct name (id ...))]{

@scheme[define-struct] will define structure constructors, selectors, mutators, and a predicate.  e.g.

@schemeblock[(define-struct foo (a b))]
defines the following forms:
@itemize[
          @item{make-foo: X Y -> foo}
           @item{foo-a: foo -> X}
           @item{foo-b: foo -> Y}
           @item{foo?: any -> boolean}
           @item{set-foo-a!: foo X -> void}
           @item{set-foo-b!: foo Y -> void}
]
}







@section{Developer details}
    
The compiler takes a ASL program and translates it to Javascript code.
Moby reimplements the ASL primitives in a Javascript runtime library
that's included with the compiled application.  (See
doc/moby-developer-api.txt for more details.)

To support smartphones, Moby uses a bridge library called Phonegap, which
provides access to the native facilities of several cell phones.  In
this way, Moby should be able to support multiple platforms with a lot
of code reuse.  Moby handles the other libraries (tilt, location, sms,
music), though with support only for the Android platforms for now.



@subsection{Dependencies}

Moby is mostly written in PLT Scheme, and the project sources are
hosted by github.com.  To develop with Moby, you will need the
following:

@itemize[
  @item{@link["http://plt-scheme.org"]{PLT Scheme} >=4.2}
   @item{@link["http://git-scm.com"]{git}}
  ]


If you wish to generate programs for the Android+Phonegap backend,
you'll also need:
@itemize[
          @item{Java >=1.6 (@url{http://java.sun.com/})}
           @item{Apache Ant >=1.7.1 (@url{http://ant.apache.org/})}
           @item{Google Android SDK >= 1.5r3 (@url{http://developer.android.com/})}
           ]

  
  
@subsection{Installing from Developer Sources}


To install Moby from the development sources:
@itemize[
    #:style 'ordered

    @item{Download the Moby source, currently hosted on 
        @hyperlink["http://github.com/dyoo/moby-scheme/tree/devel"]{github}
    and place them in your PLT Scheme collects directory.


    For example, 
@verbatim{
        $ cd ~/.plt-scheme/4.2.1/collects
        $ git clone git://github.com/dyoo/moby-scheme.git moby
}
    downloads the developer sources into a PLT Scheme user collects directory.
    
    Also, do a @exec{setup-plt -l moby} so that PLT Scheme compiles the
    Moby source code.

}
     @item{
    2.  If you're going to do Android development, make sure that
        @exec{ant} and the @exec{android} binary are in your path; Moby will use
        your PATH variable to find Apache Ant and the Android SDK.

    You can verify that @exec{android} and @exec{ant} can be found with the following:
@verbatim{
        $ which android
        /usr/local/android/tools/android

        $ which ant
        /usr/bin/ant
}
    The path values you get back may differ according to your
    system's configuration.
    

}]
    
    




@subsection{Running Moby from the command line}


You can run the Moby command line utility (@filepath{moby/src/moby.ss}) on a Moby
program to produce a compiled version of your application.  Moby
supports two output backends, both which rely on Javascript:
@itemize[
          @item{js: compiles to a web page application, which can be deployed on
      any web server.}

           @item{js+android-phonegap: compiles to an Android .apk application
      package; can also use features of the mobile platform.}
]

By default, the command line utility will use the js backend.

Let's run moby on the falling-ball.ss example in
@filepath{moby/examples/falling-ball.ss}.  We can first generate a web program.
@verbatim{
    $ cd moby/examples
    $ mred ../src/moby.ss falling-ball.ss
    $ cd FallingBall/
    $ ls
    index.html  main.js  runtime  test
}
If you open up index.html from your web browser, you should see a red
ball falling down the screen.




Furthermore, we may want to generate an Android application.
@verbatim{
    $ cd moby/examples
    $ mred ../src/moby.ss -t js+android-phonegap falling-ball.ss
    $ cd FallingBall
    $ ls
    AndroidManifest.xml  build.properties    gen               res
    assets               build.xml           libs              src
    bin                  default.properties  local.properties  tests

    $ ls bin
    classes  classes.dex  DroidGap.ap_  DroidGap-debug.apk
}

@filepath{DroidGap-debug.apk} is the compiled Android binary.  The Ant @filepath{build.xml}
build-script in the @filepath{FallingBall} directory can install, uninstall, and
reinstall the application if the Android emulator is online.
@verbatim{
    $ ant install
Buildfile: build.xml

[some output cut]

install:
     [echo] Installing bin/DroidGap-debug.apk onto default emulator...
     [exec] 1594 KB/s (120997 bytes in 0.074s)
03:38 I/ddms: Created: [Debugger 8610-->1641 inactive]
03:38 I/ddms: Good handshake from client, sending HELO to 1641
     [exec]     pkg: /data/local/tmp/DroidGap-debug.apk
     [exec] Success
03:39 I/ddms: Closing [Client pid: 1641]

BUILD SUCCESSFUL
Total time: 6 seconds
}

After this, you can look at the Android emulator, which should now
have the @filepath{FallingBall} application installed.










@subsection{Compiler}

Moby consists of the following core files for the compiler:
@itemize[
          @item{@filepath{src/compiler/beginner-to-javascript.ss}: translates Scheme programs to
    javascript programs.}

           @item{@filepath{src/compiler/env.ss}: maintains the environment structures that map
    identifiers to bindings.}

           @item{@filepath{src/compiler/permission.ss}: defines a list of capabilities that a function
    can be tagged with.}
           
           @item{@filepath{src/compiler/toplevel.ss}: maps the primitive toplevel names available in
    the base language.}
           
           @item{@filepath{src/compiler/modules.ss}: adds a few extensions to the toplevel language,
    including the reactive world primitives.}

           @item{@filepath{src/compiler/pinfo.ss}: maintains program information used to analyze a
    program and figure out what functions are used and what
    capabilities are needed.}

           @item{@filepath{src/compiler/desugar.ss}: applies syntactic transformations on programs
                  to a core form.}
           
           @item{@filepath{src/compiler/helpers.ss}: auxillary helper functions.}
]

The compiler is intentionally written in a small superset of the language
(@filepath{src/compiler/lang.ss}).  As a consequence, it is self hosting, and we take
advantage of this to produce a running compiler on the browser.
(@filepath{support/js/test/test-repl.html})

@itemize[
          @item{@filepath{src/compiler/bootstrap-js-compiler.ss}: compiles @filepath{beginner-to-javascript.ss}
                against itself to produce @filepath{support/js/compiler.js}.}]


@subsubsection{An example}

Let's see what beginner-to-javascript.ss gives us:

@verbatim{
    > (define p '((define (f x)
                    (* x x))
              
                  (f 3)))

    > (define cp (program->compiled-program p))
}

@scheme[program->compiled-program] consumes a program --- a list of
s-expressions --- and produces a @scheme[compiled-program] structure.

@verbatim{
    > cp
    #<compiled-program>
}

The compiled program consists of a list of toplevel definitions and
expressions.

@verbatim{
    > (compiled-program-defns cp)
    "\nfunction f(x) { return plt.Kernel._star_([x,x]); }"

    > (compiled-program-toplevel-exprs cp)
    > cp
    #<compiled-program>
}

The compiled program consists of a list of toplevel definitions and
expressions.

@verbatim{
    > (compiled-program-defns cp)
    "\nfunction f(x) { return plt.Kernel._star_([x,x]); }"

    > (compiled-program-toplevel-exprs cp)
    "(function (toplevel_dash_expression_dash_show0) { \n\ntoplevel_dash_expression_dash_show0((f((plt.types.Rational.makeInstance(3, 1))))); })"
}

If we want to embed the evaluation of this program in a web page, we
can use the two strings above to do so.  For convenience, we provide a
helper function @scheme[compiled-program-main] that ties both the definitions
and expression evaluation together.







@subsection{Runtime}


The Javascript program that's emitted depends on a runtime kernel
that's currently implemented in Javascript.  See the files in
@filepath{support/js/runtime}.

@subsection{Ugly Hacks}


The afterAttach() hack: one problem with excanvases in IE is that they
can't render until they're attached to a parent node.  Unfortunately,
this means that we can't render a canvas at a call to a value's
pretty-printing routine, because that value isn't yet attached to the
DOM.

This affects the implementation of plt.Kernel.BaseImage.toDomNode() We
currently add an DomNodeInserted listener to the dom node value, so it
knows when it's time to render.  However, this doesn't work under IE
(again), so we also add an afterAttach method that all of our methods
will call as soon as we add to a DOM node.  So you'll see code
sprinkled around that says:
@verbatim{
    if (node.afterAttach) { node.afterAttach(); }
}
in the following files:
@itemize[
          @item{@filepath{support/js/runtime/jsworld.js}}
           @item{@filepath{support/js/runtime/jsworld/jsworld.js}}
           @item{@filepath{support/js/runtime/types.js}}
           @item{@filepath{support/js/runtime/kernel.js}}
]



@section{Appendix}


@subsection{Bindings from ASL}

The following toplevel bindings are available from Moby, and have the
same meaning as in @hyperlink["http://docs.plt-scheme.org/htdp-langs/advanced-prim-ops.html"]{Advanced Student Language}.

@(apply itemize (map (lambda (x) (item (scheme #,x)))
                      '(*
                        +
                        -
                        /
                        <
                        <=
                        =
                        =~
                        >
                        >=
                        abs
                        acos
                        add1
                        andmap
                        angle
                        append
                        asin
                        atan
                        boolean=?
                        boolean?
                        build-list
                        caaar
                        caadr
                        caar
                        cadar
                        cadddr
                        caddr
                        cadr
                        car
                        cdaar
                        cdadr
                        cdar
                        cddar
                        cdddr
                        cddr
                        cdr
                        ceiling
                        char->integer
                        char-alphabetic?
                        char-ci<=?
                        char-ci<?
                        char-ci=?
                        char-ci>=?
                        char-ci>?
                        char-downcase
                        char-lower-case?
                        char-numeric?
                        char-upcase
                        char-upper-case?
                        char-whitespace?
                        char<=?
                        char<?
                        char=?
                        char>=?
                        char>?
                        char?
                        
                        check-expect
                        check-within
                        check-error
                        
                        complex?
                        conjugate
                        cons
                        cons?
                        cos
                        cosh
                        current-seconds
                        denominator
                        e
                        eighth
                        empty
                        empty?
                        eof
                        eof-object?
                        eq?
                        equal?
                        equal~?
                        eqv?
                        error
                        even?
                        exact->inexact
                        exp
                        expt
                        false
                        false?
                        fifth
                        first
                        floor
                        foldl
                        format
                        fourth
                        gcd
                        identity
                        imag-part
                        inexact->exact
                        inexact?
                        integer->char
                        integer?
                        lcm
                        length
                        list
                        list*
                        list->string
                        list-ref
                        log
                        magnitude
                        make-posn
                        make-string
                        map
                        max
                        member
                        memq
                        memv
                        min
                        modulo
                        negative?
                        not
                        null
                        null?
                        number->string
                        number?
                        numerator
                        odd?
                        pair?
                        pi
                        positive?
                        posn-x
                        posn-y
                        posn?
                        quotient
                        random
                        rational?
                        real-part
                        real?
                        remainder
                        rest
                        reverse
                        round
                        second
                        seventh
                        sgn
                        sin
                        sinh
                        sixth
                        sqr
                        sqrt
                        string
                        string->list
                        string->number
                        string->symbol
                        string-append
                        string-ci<=?
                        string-ci<?
                        string-ci=?
                        string-ci>=?
                        string-ci>?
                        string-copy
                        string-length
                        string-ref
                        string<=?
                        string<?
                        string=?
                        string>=?
                        string>?
                        string?
                        struct?
                        sub1
                        substring
                        symbol->string
                        symbol=?
                        symbol?
                        tan
                        third
                        true
                        zero?
                        
                        begin
                        set!
                        let
                        let*
                        letrec
                        case
                        build-vector
                        make-vector
                        vector
                        vector-length
                        vector-ref
                        vector-set!
                        vector?)))



@subsection{Unimplemented forms}
The following ASL forms are currently unimplemented:
@itemize[@item{@scheme[begin0]}
           @item{@scheme[delay]}
           @item{@scheme[shared]}
           @item{@scheme[recur]}
           @item{@scheme[when]}
           @item{@scheme[unless]}
           ]
