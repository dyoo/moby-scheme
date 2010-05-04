#lang scribble/manual

@(require (planet cce/scheme:6:0/scribble))
@(require (for-syntax (planet cce/scheme:6:0/scribble)))

@(require (for-syntax scheme/base))
@(define-syntax (mobyblock stx)
   (syntax-case stx ()
     [(header body ...)
      (let* ([source-name (syntax-source #'header)]
             [line (syntax-line #'header)]
             [column 0]
             [position (syntax-position #'header)]
             [planet-symbol 'planet]
             [moby-symbol (this-package-version-symbol)]
             [planet-symbol-length (string-length (symbol->string planet-symbol))]
             [moby-symbol-length (string-length (symbol->string moby-symbol))])
        (with-syntax ([planet-mod (datum->syntax #f 'planet (list source-name
                                                                  line 
                                                                  column 
                                                                  position 
                                                                  (string-length
                                                                   (symbol->string planet-symbol))))]
                      [lang (datum->syntax #f moby-symbol (list source-name 
                                                                 line 
                                                                 (+ column planet-symbol-length 1)
                                                                 (+ position planet-symbol-length 1) 
                                                                 moby-symbol-length))])
          (syntax/loc stx
            (schememod planet-mod lang
                       body ...))))]))


@(require (for-label (this-package-in src/moby-lang)))

@title{Moby: the Moby Scheme Compiler}

@section{What is Moby?}

Moby is a project from the PLT Scheme team.  The Moby compiler
consumes Advanced Student Language (ASL) programs that use @link["http://world.cs.brown.edu/"]{World}
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
as every second passes, the runtime sends a tick stimulus to the program.  
The page should also provide links to download packages
of the compiled program.



These programs run on the user's web browser; they can also dynamically
generate DOM trees and style them with CSS, as in the examples below.

The following example renders the world as a paragraph of text, styled
with a font-size of 30.  It uses @scheme[draw-html] and @scheme[draw-css]
to draw the web page.

@(mobyblock
(define initial-world 0)
  
(define (draw-html w)
  (list (js-p '(("id" "myPara")))
        (list (js-text "hello world"))))

(define (draw-css w)
  '(("myPara" ("font-size" "30"))))

  
(js-big-bang initial-world
             (on-draw draw-html draw-css)))


The next example shows an image and an input text field.  As with the
previous example, it uses @scheme[draw-html] and @scheme[draw-css] to
construct the web page, and every time the world changes, the runtime environment
reacts by re-drawing the web page.

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
@(declare-exporting/this-package [src/moby-lang] [])
  

@defproc[(js-big-bang (a-world world) (handlers handler?) ...) void]{
A Moby program starts a reactive computation with @scheme[js-big-bang].
The rest of the arguments hook into the reactive computation.

By default, the page that's displayed contains a rendering of the world value.
In the presence of an @scheme[on-draw] or @scheme[on-redraw] handler, @scheme[js-big-bang] will
show a customized view.

The majority of the handlers register different stimuli that can trigger changes
to the world.  One
instance is @scheme[on-tick], which registers a function to update
the world on a clock tick.
}



@defproc[(on-draw [to-dom (world -> (DOM-sexp))]
                  [to-css (world -> (CSS-sexp))]) scene]{
One of the main handlers to @scheme[js-big-bang] is @scheme[on-draw], which controls how
the world is rendered on screen.  The first argument computes a
rendering of the world as a DOM tree, and the second argument computes
that tree's styling.
}


                                                                                                                 
@defproc[(on-redraw [hook (world -> scene)]) handler?]{
For simple applications, @scheme[on-redraw] is sufficient to draw a scene onto the display.
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


@defproc[(stop-when [stop? (world -> boolean)]) handler?]{
When the world should be stopped --- when @scheme[stop?] applied to the world
produces @scheme[true] --- then the @scheme[js-big-bang] terminates.

The program:
@(mobyblock
(define (at-ten x)
  (>= x 10))

(js-big-bang 0
             (on-tick 1 add1)
             (stop-when at-ten))
)
counts up to ten and then stops.
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

The following example counts how many times a button has been clicked.
@(mobyblock
(define (press w)
  (add1 w))

(define (draw w)
  (list (js-div)
        (list (js-button press) (list (js-text "Press me")))
        (list (js-text (format "Button presses: ~a" w)))))

(define (draw-css w)
  '())

(js-big-bang 0
             (on-draw draw draw-css))
)
}
                     
                     

@defproc[(js-button! (world-update-f (world -> world))
                     (effect-f (world -> effect))
                     (attribs (listof attrib) '()))
         dom-element]{
Constructs a button.  When the button is pressed, the original world is updated,
and the original world is used to construct an effect.
}

@defproc[(js-text (text string?)) dom-element]{Constructs regular text.}

@defproc[(js-input (type string)
                   (world-update-f (or/c (world string -> world)
                                         (world boolean -> world)))
                   (attribs (listof attrib) '()))
         dom-element]{
Creates an input form element.  The types that are currently supported are:
@itemlist[@item{@scheme["text"]}
          @item{@scheme["password"]}
          @item{@scheme["checkbox"]}]
When the user changes the content of the form element, the runtime
uses @scheme[world-update-f] to update the world.  If the
@scheme[type] is either @scheme["text"] or @scheme["password"], then
the string value of the element will be passed as the second argument
to it.  If @scheme[type] is @scheme["checkbox"], a boolean
representing the checked status of the element will be passed to it.

The example below has a single text input form element, which allows the user to enter
some value.
@(mobyblock
(define (refresh w form-val)
  form-val)

(define input-node
  (js-input "text" refresh '(("id" "myname"))))

(define (draw w)
  (list (js-div)
        (list (js-div) (list (js-text (format "I see: ~s~n" w))))
        (list (js-div) (list input-node))))


(define (draw-css w)
  '())

(js-big-bang ""
             (on-draw draw draw-css))
)


The example below uses a checkbox to select among three elements:
@(mobyblock
(define (make-ingredient-checkbox-sexp ingredient)
  (local [(define (on-check w v)
            (cond
              [v
               (cons ingredient w)]
              [else
               (remove ingredient w)]))]
    (list (js-div)
          (list (js-text ingredient))
          (list (js-input "checkbox" 
	                  on-check
			  `(("value" ,ingredient)))))))

(define c1 (make-ingredient-checkbox-sexp "mushrooms"))
(define c2 (make-ingredient-checkbox-sexp "green peppers"))
(define c3 (make-ingredient-checkbox-sexp "olives"))

(define (draw w)
  (list (js-div)
        c1
        c2
        c3
        (list (js-text (format "The world is: ~s" w)))))

(define (draw-css w)
  '())

(js-big-bang '()
             (on-draw draw draw-css)))
}





@defproc[(js-img (url string) (attribs (listof attrib) '())) dom-element]{Creates an image element.
}

@defproc[(js-select (options (listof string?)) (world-update-f (world string -> world)) (attribs (listof attrib) '())) dom-element]
Constructs a select element with the given options.  Whenever a new
option is selected, the @scheme[world-update-f] function is called to
get the new world.

The example below has a select with five elements.
@(mobyblock
(define (select-house w an-option)
  an-option)

(define a-select-element
  (js-select (list ""
                   "Gryffindor"
                   "Hufflepuff" 
                   "Ravenclaw"
                   "Slytherin")
             select-house))

(define (draw w)
  (list (js-div)
        (list a-select-element)
        (list (js-text (format "House: ~a" w)))))

(define (draw-css w)
  '())

(js-big-bang ""
             (on-draw draw draw-css))
)

             
             
@subsection{Stimulus Handlers}

Stimulus handlers are provided as additional arguments to a js-big-bang.

Each stimulus has an effect-less and an effect-full version; the effect-full
version allows you to provide an effect-generating function as well as a world-updater.  When
the given stimulus emits, the old world is used to compute both
the new world and the optional effect.  Afterwards, each effect in the
effect group is applied.

@schemegrammar[effect atomic-effect
                      (listof effect)]


@deftogether[(
              @defproc[(on-tick (delay number)
                               (world-update-f (world -> world)))
               handler]
               @defproc[(on-tick! (delay number) 
                                  (world-update-f (world -> world))
                                  (effect-f (world -> effect))) 
                        handler]
               )]{
Delays by n seconds, and then calls the handlers.}


@deftogether[(
              @defproc[(on-shake (world-update-f (world -> world))) handler]
               @defproc[(on-shake! (world-update-f (world -> world))
                                   (effect-f (world -> effect)))
                        handler])]{
Calls the shake handlers when the phone is physically jerked.}


@deftogether[(
              @defproc[(on-location-change (world-update-f (world (lat number) (long number) -> world)))
                       handler]
               @defproc[(on-location-change! (world-update-f (world (lat number) (long number) -> world))
                                             (effect-f (world number number -> effect))) handler])]{
Calls the location handlers when the latitude or longitude of the
device has changed.}


@deftogether[(
              @defproc[(on-tilt! (world-update-f (world number number number -> world))
                                 (effect-f (world number number number -> effect))) handler])]{
Calls the tile handlers when the phone has been tilted.
}

@deftogether[(
              @defproc[(on-acceleration (world-update-f (world number number number -> world)))
                       handler]
               @defproc[(on-acceleration! (world-update-f (world number number number -> world))
                                          (effect-f (world number number number -> effect)))
                        handler])]{
Calls the acceleration handlers when the device feels change in acceleration.
}



@deftogether[(
              @defproc[(on-sms-receive (world-update-f (world (sender string) (message string) -> world))) handler]
               @defproc[(on-sms-receive! (world-update-f (world (sender string) (message string) -> world))
                                   (effect-f (world (sender string) (message string) -> effect)))
                        handler])]{
Calls the sms-receiving handlers when the phone is physically jerked.  The first string is the
sender's address, and the second string is the message payload.}



@subsection{Effects}

Effects allow world programs to apply side effects to the outside
world.  These are used in conjunction with the effect (@scheme[!]) version of the
stimulus handlers described above.


@defproc[(make-effect:none) effect]{No result when interpreted.}
@defproc[(make-effect:beep) effect]{Audible beep when interpreted.  On an Android smartphone, uses the notification ringtone.}


@defproc[(make-effect:play-sound (a-sound sound)) effect]{Plays a sound from the given @scheme[sound].  If the sound is already playing, then the sound continues to play.}
@defproc[(make-effect:stop-sound (a-sound sound)) effect]{Stops playing a sound from the given url.}
@defproc[(make-effect:pause-sound (a-sound sound)) effect]{Pauses a sound; if @scheme[make-effect:play-sound] for the same sound is given later, play restarts from the point where it was paused.}

A @scheme[sound] is a:
@schemegrammar[sound string
                     playlist]


@defproc[(make-effect:set-sound-volume (volume number)) effect]{Sets the sound volume; the number should be between 0 and 100.}
@defproc[(make-effect:raise-sound-volume) effect]{Raises the sound volume.  If the volume's already at 100, has no effect.}
@defproc[(make-effect:lower-sound-volume) effect]{Lowers the sound volume.  If the volume's set to 0, has no effect.}

@defproc[(make-effect:set-beep-volume (volume number)) effect]{Sets the sound volume of the beep; the number should be between 0 and 100.  On an Android smartphone, uses the notification sound.}

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


@defproc[(procedure-arity (p procedure?)) (or/c number? (list/c 'at-least number?))]{
Returns the number of arguments a procedure accepts.  If the procedure takes a minimum number of arguments,
returns the list containing @scheme['at-least] and the minimum arity.
}



@defproc[(make-hasheq) hash?]{
Creates a mutable hashtable whose keys are compared by @scheme[eq?].
}

@defproc[(make-hash) hash?]{
Creates a mutable hashtable whose keys are compared by @scheme[equal?].
}

@defproc[(hash? (x any/c)) boolean?]{
Returns @scheme[true] if @scheme[x] is a @scheme[hash], and @scheme[false] otherwise.}

@defproc[(hash-set! (a-hash hash?) (key any/c) (value any/c)) void]{
Mutates @scheme[a-hash].
}

@defproc[(hash-ref (a-hash hash?) (key any/c) (value any/c) (default-val any/c)) any/c]{
Looks up @scheme[key] in @scheme[a-hash]; if a value can't be found,
@itemize[@item{If @scheme[default-val] is a thunk, calls it and returns its value.}
	 @item{Otherwise, returns @scheme[default-val].}]
}

@defproc[(hash-remove! (a-hash hash?) (key any/c)) (void)]{
Removes a key and its associated value from @scheme[a-hash].
}

@defproc[(hash-map (a-hash hash?) (f (any/c any/c -> any/c))) (listof any/c)]{
Maps a function @scheme[f] across all the key/value pairs in
@scheme[a-hash], collecting results into a list.  The order of the
traversal is not defined.  }

@defproc[(hash-for-each (a-hash hash?) (f (any/c any/c -> any/c))) (void)]{
Applies a function @scheme[f] across all the key/value pairs in @scheme[a-hash].  The
order of the traversal is not defined.
}






  
@section{Building Android packages}

Moby supports the building of Android @filepath{.apk} packages;
programs built as Android packages can take advantage of native
features of the device.

In order to generate Android applications, the only thing you'll need
is a connection to the Internet.  When a Moby program is executed, the
third link on the web browser's page is a link to a Android .APK
package.  Clicking on the link triggers compilation on a web service,
removing the need to install any of the Android SDK tools locally.




@subsection{Android Dependencies for local compilation}

If you still wish to generate programs for the Android+Phonegap
backend using local tools, you'll need the following:

@itemize[
          @item{@link["http://java.sun.com"]{Java >=1.6}}
           @item{@link["http://ant.apache.org"]{Apache Ant >=1.7.1}}
           @item{@link["http://developer.android.com"]{Google Android SDK >= 1.5r3}}
           ]

Moby finds these by using @scheme[find-executable-path]; make sure
that @exec{ant} and the @exec{android} binary are in your path; Moby
will use your PATH variable to find Apache Ant and the Android SDK.

Note that if you're using a version of the Android SDK greater than
1.5, you must still currently include support for the 1.5 API.


You can verify that @exec{android} and @exec{ant} can be found
with the following at the interactions window:

@schemeblock[(find-executable-path "android")]
@schemeblock[(find-executable-path "ant")]

Both of these expressions must return non-false values for Moby
to be able to build packages.

Running a Moby program generates a web page with links to run the
program; once the Android SDK has been successfully installed, the
link that's named "Generate Android APK" will generate an Android
package.




@section{Underlying developer details}
    
The compiler takes a ASL program and translates it to Javascript code.
Moby reimplements the ASL primitives in a Javascript runtime library
that's included with the compiled application.  (See
doc/moby-developer-api.txt for more details.)

To support smartphones, Moby uses a bridge library called Phonegap, which
provides access to the native facilities of several cell phones.  In
this way, Moby should be able to support multiple platforms with a lot
of code reuse.  Moby handles the other libraries (tilt, location, sms,
music), though with support only for the Android platforms for now.


@subsection{Developer Dependencies}

Moby is mostly written in PLT Scheme, and the project sources are
hosted by github.com.  To develop with Moby, you will need the
following:

@itemize[
  @item{@link["http://plt-scheme.org"]{PLT Scheme >=4.2}}
   @item{@link["http://git-scm.com"]{git}}
  ]


@subsection{Setting up the Moby sources from github}
Moby is used as a PLaneT package; to install Moby from the development sources and set
up a PLaneT local link.

Download the Moby source, currently hosted on 
        @link["http://github.com/dyoo/moby-scheme/tree/devel"]{github}
    and place them somewhere convenient.
    

    For example, 
@verbatim{
        $ cd ~/work
        $ git clone git://github.com/dyoo/moby-scheme.git moby
}
    downloads the developer sources.
    
    Next, add a PLaneT local link to the moby directory:
    
@verbatim{
        $ planet link dyoo moby.plt 2 <<some-high-number>> moby
}

    with @tt{<<some-high-number>>} replaced with a number greater than the latest released version of Moby.
    
    
    Once this is done, the development link should be established, and you should be able to use
    Moby as before.


@subsection{Running the bootstrapper}

Whenever you first check out moby ,and whenever you make changes to
the underlying kernel libraries (in
@filepath{moby/support/js/runtime}), you will need to run the bootstrapper
program @filepath{moby/src/bootstrap-js-compiler.ss}.  This generates a set
of translated libraries and dependencies for the Moby runtime.



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

@;{
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
}


@section{Appendix}


@subsection{Bindings from ASL}

The following toplevel bindings are available from Moby, and have the
same meaning as in @link["http://docs.plt-scheme.org/htdp-langs/advanced-prim-ops.html"]{Advanced Student Language}.

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
			list?
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
			procedure?
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
			for-each
			printf
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
                        vector?

			when
			unless)))



@subsection{Unimplemented forms}
The following ASL forms are currently unimplemented:
@itemize[@item{@schemeid[begin0]}
           @item{@schemeid[delay]}
           @item{@schemeid[shared]}
           @item{@schemeid[recur]}
           ]
