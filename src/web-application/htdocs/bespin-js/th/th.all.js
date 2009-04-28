/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */

/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */

/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */

/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * Copyright (c) 2008, Joseph Hurst. All rights reserved.
 * Code licensed under the MIT License:
 * http://www.opensource.org/licenses/mit-license.php
 * Version 0.0.1-**forked**
 *
 * Class definition code based on Dean Edward's Base2 (MIT License) and work by
 * John Resig.
 *
 * NOTE: modifications have been made. -ben
 */
/** @ignore */
(function(){



  var initializing = false;

  var fnTest = /xyz/.test(function(){xyz;}) ?
                 /\b_super\b/ :
                 { test: function(){return true;} };

  /**
   * Global namespace for class related functions.
   * @namespace
   * @name Class
   */
  this.Class = function(){
      this.type = "Class";
  };

  /**
   * Define a new class.  In the traits model of object oriented programming
   * a class consists of:
   * <ul>
   * <li>a superclass</li>
   * <li>a set of composed traits</li>
   * <li>a collection of methods and state variables</li>
   * </ul>
   * This structure is directly mirrored in the argument structure of
   * Class.define.
   *
   * <p>
   * A number of special properties are assigned to the class at definition
   * time:
   * </p>
   * <ul>
   * <li>A static reference 'superclass' is added which points at the
   * superclass's constructor function.
   * </li>
   * <li>The static 'constructor' reference is pointed at the class itself so
   * that the 'typeof' and 'instanceof' operators will behave as expected.
   * </li>
   * <li>A static and instance method 'does' is added as well.  This method
   * behaves just like {@link Trait#does} with the notable difference that it
   * also includes any superclasses's traits in its output.
   * </li>
   * <li>Finally, an instance method '_super' is added such that invoking it in
   * any other instance method will call the first function with the same name
   * defined in the class's superclass prototype chain.
   * </li>
   * </ul>
   *
   * <p>
   * Conflicts among instance properties are resolved in the following order:
   * class members override trait members and superclass members, and trait
   * members override superclass members.
   * </p>
   *
   * <p>
   * The class constructor is specified as a method named init passed into the
   * o.members argument.
   * </p>
   *
   * @example
   * var HappyFace = Class.define({
   *   superclass: Doodle,
   *   uses: [
   *     TFace,
   *     TColoredCircle.aliases({drawColoredCircle: 'draw'})
   *   ],
   *   members: {
   *     // constructor
   *     init: function(color) {
   *       this.isDrawn = false;
   *       if (color)
   *         this.setColor(color);
   *     },
   *     // draw a happy face
   *     draw: function() {
   *       // call Doodle's draw method to set up the canvas
   *       this._super();
   *       // draw a colored circle
   *       this.drawColoredCircle();
   *       // draw the face
   *       this.drawEyes();
   *       this.drawMouth():
   *       // record that the happy face has been drawn
   *       this.isDrawn = true;
   *     },
   *     // color of the happy face (default is yellow)
   *     color: 'yellow',
   *     getColor: function() { return this.color },
   *     setColor: function(color) { this.color = color }
   *   }
   * });
   *
   * // draw a blue happy face
   * var hf = new HappyFace('blue');
   * hf.draw();
   * log(hf.isDrawn); // => true
   * log(hf.does(TFace)); // => true
   * log(HappyFace.does(TColoredCircle)); // => true
   * log(HappyFace.superclass === Doodle); // => true
   *
   * @name Class.define
   * @static
   * @function
   *
   * @throws {Trait.TraitError} Throws an error if the trait arguments are
   *    invalid, there is an unresolved conflict, or there is an unfullfilled
   *    trait requirement.
   * @return {Function} Constructor for the newly created class.
   * @param {Object} o The class configuration object.
   * @param {Function} [o.superclass] Superclass from which this class
   *    inherits.  Default superclass is the global object Class.
   * @param {Trait|Trait[]} [o.uses] A list of traits that will be composed
   *    into this class.  This happens by first constructing a new anonymous
   *    trait and then adding each of that anonymous trait's exported methods
   *    into the class prototype.  Trait methods are not copied, however, if
   *    there is a method defined at the class level with the same name
   *    (because class level methods override trait level methods).  Unlike
   *    normal trait definition, all trait requirements must be fullfilled at
   *    class definition time, either by one of the composed traits, a class
   *    method, or a superclass method.  See the documentation for o.uses in
   *    {@link Trait} for full details on how to specify this argument.
   * @param {Object} [o.members] Public instance members to be copied into this
   *    class's prototype.
   */
  Class.define = function(o) {
    if (!o.superclass) {
      o.superclass = Class;
    }

    var _super = o.superclass.prototype,
        prop = o.members || {};

    initializing = true;
    var prototype = new o.superclass();
    initializing = false;

    if (o.uses) {
      var trait = Trait.define({
        uses: o.uses,
        _klass_prototype: prototype,
        _klass_properties: prop
      });

      for (var method_name in trait._exports) {
        if (!trait._exports.hasOwnProperty(method_name)) continue;

        if (prop.hasOwnProperty(method_name)) {
          if (typeof prop[method_name] != "function") {
            throw new Trait.TraitError(method_name +
                " overrides trait method and must be a function.");
          }
        } else {
          prop[method_name] = trait._exports[method_name];
        }
      }
    }

    for (var name in prop) {
      if (!prop.hasOwnProperty(name)) continue;

      prototype[name] = typeof prop[name] == "function" &&
        typeof _super[name] == "function" && fnTest.test(prop[name]) ?
        (function(name, fn){
          return function() {
            var tmp = this._super;

            this._super = _super[name];

            var ret = fn.apply(this, arguments);
            this._super = tmp;

            return ret;
          };
        })(name, prop[name]) :
        prop[name];
    }

    var klass = function() {
      if ( !initializing && this.init ) {
        this.init.apply(this, arguments);
      }
    }

    klass.prototype = prototype;

    klass.constructor = klass;

    klass.superclass = o.superclass;

    klass.prototype.type = o.type;

    klass.does = function(trait_ref) {
      if (!trait) {
        if (klass.superclass.does)
          return klass.superclass.does(trait_ref);
        if (trait_ref)
          return false;
        return [];
      }

      var used_traits = trait.does(trait_ref);
      if (klass.superclass.does) {
        if (trait_ref)
          return used_traits || klass.superclass.does(trait_ref);
        var inherited_traits = klass.superclass.does(trait_ref).concat([]);
        for (var i = used_traits.length-1; i >= 0; i--) {
          if (inherited_traits.indexOf(used_traits[i]) === -1)
            inherited_traits.push(used_traits[i]);
        }
        return inherited_traits;
      }

      return used_traits;
    };
    if (!klass.prototype.hasOwnProperty("does")) {
      klass.prototype.does = klass.does;
    }

    return klass;
  };



  function isArray(o) {
    return Object.prototype.toString.apply(o) === '[object Array]';
  }

  function makeArray(o) {
    if (o)
      return isArray(o) ? o : [o];
    return [];
  }

  function stringArrayToHash(a) {
    if (!a) return {};
    var ret = {};
    if (!isArray(a)) {
      ret[a] = true;
      return ret;
    }
    for (var i = a.length-1; i >=0; i--)
      ret[a[i]] = true;
    return ret;
  }

  function merge(receiver, sender) {
    for (var i in sender) {
      if (!sender.hasOwnProperty(i)) continue;
      receiver[i] = sender[i];
    }
    return receiver;
  }

  this.Trait = Class.define({
    members: /** @lends Trait.prototype */ {
      /**
       * <p>
       * A trait is a group of pure methods that serves as a building block for
       * classes and is a primitive unit of code reuse. In this model, classes
       * are composed from a set of traits by specifying glue code that
       * connects the traits together and accesses the necessary state.  If you
       * are unfamiliar with the general object oriented programming notion of
       * a trait it would serve you well to check out the
       * <a href="http://code.google.com/p/jstraits/wiki/TraitSynopsis">synopsis
       * and examples</a> before reading the rest of this documentation.
       * </p>
       *
       * <p>
       * The constructor creates a new trait for use in other traits or classes.
       * The factory method {@link Trait.define} is the preferred way to
       * instantiate new traits, as opposed to calling 'new Trait(...)'
       * directly.
       * </p>
       *
       * @example
       * var TColoredCircle = new Trait({
       *   uses: [TColor, TCircle.aliases({'drawOutline': 'draw'})],
       *   requires: 'fillWithColor',
       *   methods: {
       *     draw: function() {
       *       // draw a colored circle
       *       this.drawOutline();
       *       this.fillWithColor(this.getColor());
       *     }
       *   }
       * });
       *
       * @constructs
       * @see Trait.define
       * @throws {Trait.TraitError} Throws an error if the trait definition
       *    arguments are invalid, the trait definition is inconsistent, or
       *    there is an unresolved conflict.
       * @param {Object} o The trait configuration object.
       * @param {Trait|Trait[]} [o.uses] Other trait(s) that will be composed
       *    into this new trait.  Note that trait composition is both
       *    associative and commutative, so if specifying more than one trait
       *    the order does not matter.  To alias or exclude methods from
       *    subtraits as they are composed, call {@link Trait#aliases} or
       *    {@link Trait#excludes}.  Calls to these functions may be chained.
       *    Passing a trait into o.uses causes the methods from that trait
       *    (plus any aliases and modulo any excludes) to be added to the new
       *    trait.  If any of these exported method names conflict with another
       *    trait specified in o.uses, the conflict must be resolved (unless
       *    the conflicting method names point to the exact same function).
       *    Conflicts may be resolved by either 1) overriding the method in
       *    o.methods 2) overriding the method at the class level (if this
       *    trait is being defined as part of a class) or 3) excluding all but
       *    one of the conflicting methods.
       * @param {String|String[]} [o.requires] A list of method names that must
       *    be implemneted before this trait can function properly.  A trait
       *    may have open requirements, but all its requirements must be
       *    fulfilled when it is composed into a class.  Requirements can be
       *    satisfied by other traits or class methods.
       * @param {Object} [o.methods] A dictionary of methods that this trait
       *    exports (in addition to those composed in o.uses).  Methods may
       *    access instance methods, but should not directly access instance
       *    variables.  References to instance methods that are not deinfed in
       *    this trait or a composed subtrait should have their method names
       *    placed in the o.requires parameter.
       */
      init: function(o) {
        this._subtraits = makeArray(o.uses);
        this._requires = stringArrayToHash(o.requires);
        this._exports = o.methods ? o.methods : {};

        var method_name;
        for (method_name in this._requires) {
          if (!this._requires.hasOwnProperty(method_name)) continue;
          if (this._exports.hasOwnProperty(method_name)) {
            throw new Trait.TraitError("Trait cannot require and provide " +
              "the same method " + method_name);
          }
        }

        var subtrait, exports, i, excludes = {};
        for (i = this._subtraits.length-1; i >= 0; i--) {
          subtrait = this._subtraits[i];

          exports = merge({}, subtrait._exports);
          if (subtrait._aliases)
            merge(exports, subtrait._aliases);

          if (subtrait._excludes) {
            for (method_name in subtrait._excludes) {
              if (!subtrait._excludes.hasOwnProperty(method_name)) continue;
              delete exports[method_name];
              merge(excludes, subtrait._excludes);
            }
          }

          for (method_name in exports) { // each exported method
            if (!exports.hasOwnProperty(method_name)) continue;
            if (o._klass_prototype &&
                o._klass_properties.hasOwnProperty(method_name) &&
                typeof o._klass_properties[method_name] == "function")
            {
              continue;
            }

            if (this._exports.hasOwnProperty(method_name)) {
              if ((!o.methods || !o.methods.hasOwnProperty(method_name)) &&
                 this._exports[method_name] != exports[method_name])
              {
                throw new Trait.TraitError("Multiple subtraits provide " +
                   "method " + method_name + " creating a conflict. " +
                   "Exclude all but one of the methods or override the " +
                   "method in the trait/class to resolve.");
              }
              continue;
            }

            this._exports[method_name] = exports[method_name];
          }

          for (method_name in subtrait._requires) {
            if (!subtrait._requires.hasOwnProperty(method_name)) continue;
            if (!this._exports.hasOwnProperty(method_name)) {
              this._requires[method_name] = true;
            }
          }

          delete subtrait._aliases
          delete subtrait._excludes
        }

        for (method_name in excludes) {
          if (!excludes.hasOwnProperty(method_name)) continue;
          if (!this._exports.hasOwnProperty(method_name) &&
              (!o._klass_prototype ||
               (typeof o._klass_prototype[method_name] != "function" &&
                typeof o._klass_properties[method_name] != "function"
               )))
          {
            throw new Trait.TraitError("Excluded method " + method_name +
                " must be provided by another class or trait.");
          }
        }

        for (method_name in this._requires) {
          if (!this._requires.hasOwnProperty(method_name)) continue;
          if (this._exports.hasOwnProperty(method_name)) {
            delete this._requires[method_name];
          } else if (o._klass_prototype &&
                     (typeof o._klass_prototype[method_name] != "function" &&
                      typeof o._klass_properties[method_name] != "function"
                     ))

          {
            throw new Trait.TraitError("Trait requirement " + method_name +
                " not fullfilled by class.");
          }
        }
      },

      /**
       * Alias a method to a different name during trait composition.  Aliases
       * should only be used in a 'uses' clause inside a class or trait
       * definition.  It may be chained with {@link Trait#excludes}.  Aliasing
       * a method causes it to be copied under the new alias name in addition
       * to the original method name.  Multiple aliases may be made to the
       * same function.  Aliases are treated exactly like normal method names
       * during trait composition.
       *
       * @throws {Trait.TraitError} Throws a TraitError if attempting to alias a
       *     non-existent function, create an alias with the same name as a
       *     natively exported method, or create an alias with the same name
       *     as one of the required method names.
       * @return {Trait} this
       * @param {Object} o A String to String mapping of alias names to exported
       *     method names.
       */
      aliases: function(o) {
        this._aliases = this._aliases || {};
        for (var alias in o) {
          if (!o.hasOwnProperty(alias)) continue;
          if (!this._exports.hasOwnProperty(o[alias]))
            throw new Trait.TraitError("can't alias " + alias
                + " to " + o[alias] +
                " because trait doesn't have that method");
          if (this._exports.hasOwnProperty(alias))
            throw new Trait.TraitError("can't create an alias with name " +
                alias + " because trait natively exports that method");
          if (this._requires.hasOwnProperty(alias))
            throw new Trait.TraitError("can't create an alias with name " +
                alias + " because trait requires method with same name");
          this._aliases[alias] = this._exports[o[alias]];
        }
        return this;
      },

      /**
       * Exclude a method during trait composition.  Excludes should only be
       * used in a 'uses' clause inside a class or trait definition.  It may be
       * chained with {@link Trait#aliases}.  Excluding a method causes it to
       * not be copied into the containing class or trait as it normally would.
       * If a method is excluded a method with the same name must be provided,
       * either by another trait or a class method.
       *
       * @throws {Trait.TraitError} Throws a TraitError if attempting to exclude
       *     a method that is not exported by this trait.
       * @returns {Trait} this
       * @param {String|String[]} a Method(s) to exclude during trait
       *     composition.
       */
      excludes: function(a) {
        this._excludes = this._excludes || {};
        a = makeArray(a);
        for (var i = a.length-1; i >=0; i--) {
          if (!this._exports.hasOwnProperty(a[i])) {
            throw new Trait.TraitError("can't exclude method " + a[i] +
                " because no such method exists in trait");
          }
          this._excludes[a[i]] = true;
        }
        return this;
      },

      /**
       * Inspect all traits used by this trait.  Note that this trait is
       * included in the list of what this trait 'does'. If no argument is
       * passed, an array of all traits is returned.  If a trait is passed, a
       * boolean is returned indicating if the specified trait is one of the
       * composed traits.  This method differs from {@link Trait#subtraits} in
       * that subtraits only checks for traits specified in the use clause,
       * while this method recursively checks to see if any of the subtraits'
       * subraits match, and so on.
       *
       * @return {Trait[]|Boolean} List of all traits composed into this trait
       *     or a boolean indicating if a particular trait was used.
       * @param {Trait} [trait_ref] Trait to check for inclusion in the list
       *     of composed traits.
       */
      does: function(trait_ref) {
        if (!this._does) {
          this._does = [this].concat(this._subtraits);
          var i, j, subsub;
          for (i = this._subtraits.length-1; i >= 0; i--) {
            subsub = this._subtraits[i].does();
            for (j = subsub.length-1; j >= 0; j--) {
              if (this._does.indexOf(subsub[j]) === -1)
                this._does.push(subsub[j]);
            }
          }
        }

        if (trait_ref)
          return this._does.indexOf(trait_ref) >= 0;
        return this._does;
      },

      /**
       * Inspect method names required by this trait.  If no arguments are
       * passed, an object with keys representing all the required methods is
       * returned.  If a string argument is given, requires returns a boolean
       * indicating if this trait requires a method with that name.
       *
       * @return {Object|Boolean} Object keyed by required method names or
       *     boolean indicating if a particular method name is required.
       * @param {String} method_name Method name to check if in required method
       *     name list.
       */
      requires: function(method_name) {
        if (method_name)
          return this._requires.hasOwnProperty(method_name) &&
            this._requires[method_name];
        return this._requires;
      },

      /**
       * Inspect subtraits used by this trait. Note that only immediate
       * subtraits are dealt with here (i.e. those passed in the 'uses'
       * clause). To recursively check if a trait uses another trait see
       * {@link Trait#does}.  If no argument is passed, an array of all
       * subtraits is returned.  If a trait is passed, a boolean is returned
       * indicating if the specified trait is one of the subtraits.
       *
       * @return {Trait[]|Boolean} List of all subtraits or boolean indicating
       *     if a particular subtrait was used.
       * @param {Trait} [trait_ref] Trait to check for inclusion in the list
       *     of subtraits.
       */
      subtraits: function(trait_ref) {
        if (trait_ref)
          return this._subtraits.indexOf(trait_ref) >= 0;
        return this._subtraits;
      },

      /**
       * Inspect methods exported by this trait.  If no arguments are passed,
       * an object mapping each method name exported by this trait to its
       * associated function is returned.  If a string argument is given,
       * methods checks if this trait exports a method with that name.  If so
       * it returns the associated function, otherwise it returns undefined.
       *
       * @return {Object|Function} Mapping of method names to functions, a
       *     specific function, or undefined.
       * @param {String} [method_name] Name of the method to look up in this
       *     trait's method export list.
       */
      methods: function(method_name) {
        if (method_name)
          return this._exports.hasOwnProperty(method_name) &&
            this._exports[method_name];
        return this._exports;
      }
    }
  });

  /**
   * Factory method to create new traits.  Arguments are the same as those
   * passed to the {@link Trait} constructor.  This static method is the
   * preferred way to create new traits.
   *
   * @example
   * var TColoredCircle = Trait.define({
   *   uses: [TColor, TCircle.aliases({'drawOutline': 'draw'})],
   *   requires: 'fillWithColor',
   *   methods: {
   *     draw: function() {
   *       // draw a colored circle
   *       this.drawOutline();
   *       this.fillWithColor(this.getColor());
   *     }
   *   }
   * });
   *
   * @static
   * @memberOf Trait
   * @name define
   * @function
   * @see Trait
   * @return {Trait}
   * @throws {Trait.TraitError}
   * @param {Object} o
   * @param {Trait|Trait[]} [o.uses]
   * @param {String|String[]} [o.requires]
   * @param {Object} [o.methods]
   */
  Trait.define = function(o) {
    return new Trait(o);
  };

  Trait.TraitError = Class.define({
    superclass: Error,
    members: /** @lends Trait.TraitError.prototype */ {
      /**
       * Generic error thrown for any trait related exceptions.
       *
       * @constructs
       * @augments Error
       * @param {String} msg The message to show when printing out the string.
       */
      init: function(msg) {
        this.name = "TraitError";
        this.message = msg;
      }
    }
  });
})();


if (!("console" in window)) {
  window.console = {
    log: function(s) {
    }
  }
}

if (location.href.indexOf("file:") == 0) {
    try {
       if (netscape.security.PrivilegeManager.enablePrivilege) {
           netscape.security.PrivilegeManager.enablePrivilege('UniversalBrowserRead');
       }
    } catch (ex) {
        console.log("Couldn't elevate priviledges to deal with file URLs");
    }
}

if (typeof th == "undefined") th = {};

th.browser = {
  IE:     !!(window.attachEvent && !window.opera),
  Opera:  !!window.opera,
  WebKit: navigator.userAgent.indexOf('AppleWebKit/') > -1,
  Gecko:  navigator.userAgent.indexOf('Gecko') > -1 && navigator.userAgent.indexOf('KHTML') == -1,
  MobileSafari: !!navigator.userAgent.match(/Apple.*Mobile.*Safari/)
}

th.fixCanvas = function(ctx) {
    if (!ctx.fillText && ctx.mozDrawText) {
        ctx.fillText = function(textToDraw, x, y, maxWidth) {
            ctx.translate(x, y);
            ctx.mozTextStyle = ctx.font;
            ctx.mozDrawText(textToDraw);
            ctx.translate(-x, -y);
        }
    }

    if (!ctx.measureText && ctx.mozMeasureText) {
        ctx.measureText = function(text) {
            if (ctx.font) ctx.mozTextStyle = ctx.font;
            var width = ctx.mozMeasureText(text);
            return { width: width };
        }
    }

    if (ctx.measureText && !ctx.html5MeasureText) {
        ctx.html5MeasureText = ctx.measureText;
        ctx.measureText = function(text) {
            var textMetrics = ctx.html5MeasureText(text);

            textMetrics.ascent = ctx.html5MeasureText("m").width;

            return textMetrics;
        }
    }

    if (!ctx.fillText) {
        ctx.fillText = function() {}
    }

    if (!ctx.measureText) {
        ctx.measureText = function() { return 10; }
    }

    return ctx;
}

th.byId = function(id) {
    return document.getElementById(id);
}

th.isString = function(it) {
	return !!arguments.length && it != null && (typeof it == "string" || it instanceof String); // Boolean
}

th.isArray = function(it) {
	return it && (it instanceof Array || typeof it == "array"); // Boolean
}

th.forEach = function(arr, callback, thisObject) {
    if (!arr || !arr.length){ return; }

    var _p = th._getParts(arr, thisObject, callback); arr = _p[0];
    for (var i = 0, l = arr.length; i < l; ++i) {
        _p[2].call(_p[1], arr[i], i, arr);
    }
}

th._getParts = function(arr, obj, cb){
    return [
        th.isString(arr) ? arr.split("") : arr,
        obj || this,
        th.isString(cb) ? new Function("item", "index", "array", cb) : cb
    ];
};

th.xhrGet = function(args) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {
            args.load.apply(args.context, [ xhr.responseText ]);
        }
    };
    xhr.open("GET", args.url);
    xhr.send(null);
}

th.remove = function(array, toRemove) {
    var newarr = [];
    for (var i = 0; i < array.length; i++) {
        if (array[i] != toRemove) newarr.push(array[i]);
    }
    return newarr;
}

th.observe = function(source, eventName, listener, context) {
    var toInvoke = (context) ? function(e) { listener.apply(context, [e]) } : listener;
    source["on" + eventName] = toInvoke;
}

th.trim = String.prototype.trim ?
	function(str){ if (str == undefined) return str; return str.trim(); } :
	function(str){ if (str == undefined) return str; return str.replace(/^\s\s*/, '').replace(/\s\s*$/, ''); };

th.mixin = function(obj, props) {
    if (!obj) { obj = {}; }
    for(var i = 1, l = arguments.length; i < l; i++){
        th._mixin(obj, arguments[i]);
    }
    return obj;
}

th._mixin_tobj = {};
th._mixin = function(obj, props) {
    for (var x in props) {
        if (th._mixin_tobj[x] === undefined || th._mixin_tobj[x] != props[x]) {
            obj[x] = props[x];
        }
    }

    if (th.browser.IE && props) {
        var p = props.toString;
        if (typeof p == "function" && p != obj.toString && p != th._mixin_tobj.toString &&
                p != "\nfunction toString() {\n    [native code]\n}\n"){
            obj.toString = props.toString;
        }
    }

    return obj;
}

th.cumulativeOffset = function(element) {
    var valueT = 0, valueL = 0;
    do {
        valueT += element.offsetTop  || 0;
        valueL += element.offsetLeft || 0;
        element = element.offsetParent;
    } while (element);
    return th._returnOffset(valueL, valueT);
}

th._returnOffset = function(l, t) {
    var result = [l, t];
    result.left = l;
    result.top = t;
    return result;
};

th.isPercentage = function(str) {
    return (str.indexOf && str.indexOf("%") != -1);
};

th.isCssPixel = function(str) {
    str = th.trim(str).toLowerCase();
    return (str.indexOf("px") == str.length - 2);
};

th.isCssLength = function(str) {
    return /^[\d\.]+(em|ex|px|\%|in|cm|mm|pt|pc)$/.test(str);
};

th.isCssBorderStyle = function(str) {
    return /^(none|hidden|dotted|dashed|solid|double|groove|ridge|inset|outset)$/.test(str);
};

th.defaultCSSBorder = function() {
    return {
        top: {
            width: 0,
            style: "none",
            color: "black"
        },
        bottom: {
            width: 0,
            style: "none",
            color: "black"
        },
        left: {
            width: 0,
            style: "none",
            color: "black"
        },
        right: {
            width: 0,
            style: "none",
            color: "black"
        }
    };
};

th.defaultCSSMargin = function() {
    return {top: 0, bottom: 0, left: 0, right: 0};
};

th.defaultCSSPadding = function() {
    return {top: 0, bottom: 0, left: 0, right: 0};
};

th.getSpaceDelimitedItems = function(input) {
    return th.parenAwareSplit(" ", input);
}

th.getCommaDelimitedItems = function(input) {
    return th.parenAwareSplit(",", input);
}

th.parenAwareSplit = function(delim, input) {
    if (delim.length != 1) throw "Invalid delimiter passed to parenAwareSplit: '" + delim + "'";

    var pieces = [];
    var inParens = 0;
    var currentCharacter;
    var currentPiece = "";

    for (var x = 0; x < input.length; x++) {
       currentCharacter = input[x];

       if (currentCharacter == delim) {
           if (!inParens) {
               pieces.push(th.trim(currentPiece));
               currentPiece = "";
               continue;
           }
       } else if (currentCharacter == '(') {
           inParens++;
       } else if (currentCharacter == ')') {
           inParens--;
       }
       currentPiece += currentCharacter;
    }

    pieces.push(th.trim(currentPiece)); // get the last piece too

    return pieces;
}

th.whitespace = " \t\n\r";
th.isWhitespace = function(str) {
    if (!str || str.length == 0) return true;

    for (var i = 0; i < str.length; i++) {
        var c = str.charAt(i);

        if (th.whitespace.indexOf(c) == -1) return false;
    }

    return true;
}

th.measureUsingDOM = function(measuringFunction, context) {
    if (!document) throw "Can't perform measurements outside of browser";

    var body = document.getElementsByTagName("body")[0];
    var divvy = document.createElement("div");
    divvy.setAttribute("style", "position: absolute; visibility:hidden");
    body.appendChild(divvy);

    if (context) {
        measuringFunction.apply(context, [ divvy ]);
    } else {
        measuringFunction(divvy);
    }

    body.removeChild(divvy);
}

th.measure_cache = {};   // speculative cache, haven't measured to see if it's useful

th.safeget = function(value, def) {
    return (value == undefined) ? def : value;
}

th.unitRegex = /^([\d\.]+)(em|ex|px|\%|in|cm|mm|pt|pc)$/;
th.absoluteUnitRegex = /^([\d\.]+)(px|in|cm|mm|pt|pc)$/;

th.isBorderLength = function(length) {
    return (length == "thin" || length == "medium" || length == "thick");
}

th.convertLengthToPixels = function(length, component) {
    if (th.isBorderLength(length)) {
        return th.convertBorderLengthToPixels(length);
    } else if (th.absoluteUnitRegex.test(length)) {
        return th.convertAbsoluteUnitToPixels(length);
    }
}

th.convertBorderLengthToPixels = function(length) {
    if (length === undefined) return 0;

    var cacheKey = length;
    if (th.measure_cache[cacheKey]) return th.measure_cache[cacheKey];

    var result = undefined;
    th.measureUsingDOM(function(div) {
        div.setAttribute("style", "position: relative; border-left: " + length + " solid black");
        div.innerHTML = "some text";
        result = div.scrollWidth - div.clientWidth;
    });

    th.measure_cache[cacheKey] = result;
    return result;
}

th.convertAbsoluteUnitToPixels = function(lengthStr, length) {
    var unit;
    if (length === undefined) {
        var results = th.absoluteUnitRegex.exec(lengthStr);
        if (results.length != 3) throw "Passed length " + lengthStr + " isn't a CSS absolute unit";
        unit = results[2];
        length = Number(results[1]);
    } else {
        unit = lengthStr;
    }

    if (unit == "px") {
        return length;
    } else if (unit == "in" || unit == "cm" || unit == "mm" || unit == "pt" || unit == "pc") {
        return th.measureAbsoluteUnit(length + unit);
    } else {
        throw "Unsupported unit: " + unit;
    }
}

th.measureAbsoluteUnit = function(measurement) {
    if (measurement === undefined) return 0;

    var cacheKey = measurement;
    if (th.measure_cache[cacheKey]) return th.measure_cache[cacheKey];

    var result = undefined;
    th.measureUsingDOM(function(div) {
        var style = div.getAttribute("style");
        div.setAttribute("style", style += "; width: " + measurement);
        result = div.scrollWidth;
    });
    th.measure_cache[cacheKey] = result;
    return result;
}

th.convertEmToPixels = function(fontStyle, length) {
    var cacheKey = fontStyle + length;
    if (th.measure_cache[cacheKey]) return th.measure_cache[cacheKey];
    var result = undefined;
    th.measureUsingDOM(function(div) {
        var style = div.getAttribute("style");
        div.setAttribute("style", style += "; font: " + fontStyle + "; width: " + length + "em");
        result = div.scrollWidth;
    });
    th.measure_cache[cacheKey] = result;
    return result;
}

th.getHierarchyString = function(component) {
    var hierarchy = "";
    while (component) {
        var compy = component.type;
        if (component.id) compy += "#" + component.id;
        if (component.className) compy += "." + component.className;
        hierarchy = compy += " " + hierarchy;
        component = component.parent;
    }
    return th.trim(hierarchy);
}

th.isObject = function(/*anything*/ it){
	return it !== undefined &&
		(it === null || typeof it == "object" || th.isArray(it) || th.isFunction(it)); // Boolean
}

th.isFunction = (function(){
	var _isFunction = function(/*anything*/ it){
		return it && (typeof it == "function" || it instanceof Function); // Boolean
	};

    return _isFunction;
})();

if (typeof th == "undefined") th = {};

th.CssParser = Class.define({
    members: {
        parse: function(str, ret) {
            if (!ret) ret = {};

            str = str.replace(/\s+/gi, " ");

            th.forEach(this.munge(str, false).split('`b%'), function(css){
                css = css.split('%b`'); // css[0] is the selector; css[1] is the index in munged for the cssText
                if (css.length < 2) return; // invalid css
                css[0] = this.restore(css[0]);
                var obj = ret[css[0]] || {};
                ret[css[0]] = th.mixin(obj, this.parsedeclarations(css[1]));
            }, this);

            return ret;
        },

        REbraces: /{[^{}]*}/,

        REfull: /\[[^\[\]]*\]|{[^{}]*}|\([^()]*\)|function(\s+\w+)?(\s*%b`\d+`b%){2}/, // match pairs of parentheses, brackets, and braces and function definitions.

        REatcomment: /\/\*@((?:[^\*]|\*[^\/])*)\*\//g, // comments of the form /*@ text */ have text parsed

        REcomment_string:
          /(?:\/\*(?:[^\*]|\*[^\/])*\*\/)|(\\.|"(?:[^\\\"]|\\.|\\\n)*"|'(?:[^\\\']|\\.|\\\n)*')/g,

        REmunged: /%\w`(\d+)`\w%/,

        uid: 0, // unique id number

        munged: {}, // strings that were removed by the parser so they don't mess up searching for specific characters

        munge: function(str, full) {
            var self = this;
            str = str
                .replace(this.REatcomment, '$1') // strip /*@ comments but leave the text (to let invalid CSS through)
                .replace(this.REcomment_string, function(s, string) { // strip strings and escaped characters, leaving munged markers, and strip comments
                    if (!string) return '';
                    var replacement = '%s`'+(++self.uid)+'`s%';
                    self.munged[self.uid] = string.replace(/^\\/, ''); // strip the backslash now
                    return replacement;
                });

            var RE = full ? this.REfull : this.REbraces;
            while (match = RE.exec(str)) {
                replacement = '%b`'+(++this.uid)+'`b%';
                this.munged[this.uid] = match[0];
                str = str.replace(RE, replacement);
            }
            return str;
        },

        restore: function(str) {
            if (str === undefined) return str;
            while (match = this.REmunged.exec(str)) {
                str = str.replace(this.REmunged, this.munged[match[1]]);
            }
            return th.trim(str);
        },

        parsedeclarations: function(index){ // take a string from the munged array and parse it into an object of property: value pairs
            var str = this.munged[index].replace(/(?:^\s*[{'"]\s*)|(?:\s*([^\\])[}'"]\s*$)/g, '$1'); // find the string and remove the surrounding braces or quotes
            str = this.munge(str); // make sure any internal braces or strings are escaped
            var parsed = {};
            th.forEach(str.split(';'), function(decl) {
                decl = decl.split(':');
                if (decl.length < 2) return;
                parsed[this.restore(decl[0])] = this.restore(decl[1]);
            }, this);
            return parsed;
        }
    }
});
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */


if (typeof th == "undefined") th = {};

th.EventHelpers = new Trait({
    methods: {
        wrapEvent: function(e, root) {
            var x = e.clientX - th.cumulativeOffset(this.canvas).left;
            var y = e.clientY - th.cumulativeOffset(this.canvas).top;

            var component = root.getComponentForPosition(x, y, true);
            e.thComponent = component;

            this.addComponentXY(e, root, component);
        },

        addComponentXY: function(e, source, dest) {
            if (!dest.bounds) {
                console.log("No dest bounds - " + dest.type);
                console.log(dest.bounds);
                console.log(dest);
                return;
            }

            var x = e.clientX - th.cumulativeOffset(this.canvas).left;
            var y = e.clientY - th.cumulativeOffset(this.canvas).top;

            var nxy = { x: x, y: y };

            var c = dest;
            while (c) {
                nxy.x -= c.bounds.x;
                nxy.y -= c.bounds.y;
                c = c.parent;

                if (c == source) {
                    e.componentX = nxy.x;
                    e.componentY = nxy.y;
                    return;
                }
            }
        }
    }
});

th.StringHelpers = new Trait({
    methods: {
        isPercentage: function(value) {
            return value.indexOf && value.indexOf("%") != -1;    // TODO: make more robust
        }
    }
});

th.ComponentHelpers = new Trait({
    methods: {
        d: function() {
            var insets = this.getInsets(); // better to calculate the insets once
            return {
               b: (this.bounds) ? { x: this.bounds.x, y: this.bounds.y, w: this.bounds.width, h: this.bounds.height,
                                    iw: this.bounds.width - insets.left - insets.right,
                                    ih: this.bounds.height - insets.top - insets.bottom } : {},
               i: { l: insets.left, r: insets.right, t: insets.top, b: insets.bottom,
                    w: insets.left + insets.right, h: insets.top + insets.bottom }
            }
        },

        shouldPaint: function() {
            return (this.shouldLayout() && this.cssValue("visibility") != "hidden");
        },

        shouldLayout: function() {
            return (this.cssValue("display") != "none");
        },

        emptyInsets: function() {
            return { left: 0, right: 0, bottom: 0, top: 0 };
        },

        resolveCss: function() {
            var resources = th.global_resources;

            var declarations;

            var cacheKey = th.getHierarchyString(this);
            if (resources.resolvedCssCache[cacheKey]) {
                declarations = resources.resolvedCssCache[cacheKey];
            } else {
                declarations = {};

                var propertyName;
                var sheetTypes = [ "userAgentCss", "userCss", "authorCss" ];
                for (var i = 0; i < sheetTypes.length; i++) {
                    var currentSheet = sheetTypes[i];
                    th.forEach(resources[currentSheet], function(css) {
                        for (var selector in css) {
                            var selectorPieces = selector.split(",");
                            for (var s = 0; s < selectorPieces.length; s++) {
                                var selectorPiece = th.trim(selectorPieces[s]);
                                var specificity = this.getSpecificity(selectorPiece);

                                var pseudos = selectorPiece.split(":");
                                var selectorBit = pseudos[0];
                                pseudos = (pseudos.length > 1) ? pseudos.slice(1) : [ "(default)" ];

                                if (this.matchesSelector(selectorBit)) {
                                    var properties = css[selector];

                                    for (propertyName in properties) {
                                        for (var i = 0; i < pseudos.length; i++) {
                                            var pseudoBit = pseudos[i];
                                            if (declarations[pseudoBit] === undefined) declarations[pseudoBit] = {};

                                            var prop = {
                                                value: properties[propertyName],
                                                selector: selectorPiece,
                                                specificity: specificity
                                            };

                                            if (declarations[pseudoBit][propertyName]) {
                                                prop = this.getSpecificityWinner(prop, declarations[pseudoBit][propertyName]);
                                            }

                                            declarations[pseudoBit][propertyName] = prop;
                                        }
                                    }
                                }
                            }
                        }
                    }, this);
                }

                resources.resolvedCssCache[cacheKey] = declarations;
            }

            this.styles = {};
            for (var pseudoBit in declarations) {
                for (var declaration in declarations[pseudoBit]) {
                    if (this.styles[pseudoBit] === undefined) this.styles[pseudoBit] = {};

                    this.styles[pseudoBit][declaration] = declarations[pseudoBit][declaration].value;
                }
            }

            this.refreshCss = false;
        },

        matchesSelector: function(selector) {
            var s = selector.toLowerCase();

            if (s == "*") return true;

            if (s.indexOf(">") != -1) {
                var ss = s.split(">");

                if (ss.length != 2) {
                    console.log("unsupported child selector syntax; must be SEL1 > SEL2, was '" + selector + "'");
                    return false;
                }

                if (this.matchesSelector(th.trim(ss[1]))) {
                    if (!this.parent) return false;
                    return (this.parent.matchesSelector(th.trim(ss[0])));
                }

                return false;
            }

            if (s.indexOf(" ") != -1) {
                var ss = s.split(" ");

                if (ss.length != 2) {
                    console.log("unsupported ancestor selector syntax; must be SEL1 SEL2, was '" + selector + "'");
                    return false;
                }

                if (this.matchesSelector(th.trim(ss[1]))) {
                    var ancestor = this.parent;
                    while (ancestor) {
                        if (ancestor.matchesSelector(th.trim(ss[0]))) return true;
                        ancestor = ancestor.parent;
                    }

                    return false;
                }
            }

            if (s.indexOf(".") == 0) {
                if (!this.className) return false;
                if (this.className.toLowerCase() == s.substring(1)) return true;
            }

            if (s.indexOf("#") == 0) {
                if (!this.id) return false;
                return ("#" + this.id) == s;
            }

            var type = this.type.toLowerCase();
            if (type == s) return true;

            if (this.id && (s == (type + "#" + this.id))) return true;

            if (this.className && (s == (type + "." + this.className.toLowerCase()))) return true;

            return false;
        },

        getSpecificity: function(selector, isLocal) {
            var s = { a: 0, b: 0, c: 0, d: 0 };

            if (isLocal) s.a = 1;   // "style" attribute

            var pieces = th.getSpaceDelimitedItems(selector);
            for (var i = 0; i < pieces.length; i++) {
                var p = pieces[i];
                if (p == "*" || p == "+" || p == ">") continue;

                if (p.indexOf("#") == 0) {
                    s.b += 1;
                    continue;
                }

                s.c += p.split(".").length - 1; // number of attributes in the selector

                var pseudo = p.split(":");
                if (pseudo.length == 2) {
                    if (pseudo[1] == "first-line" || pseudo[1] == "first-letter" || pseudo[1] == "before" || pseudo[1] == "after") {
                        s.d += 1;
                    } else {
                        s.c += 1;
                    }
                }
            }

            return s;
        },

        getSpecificityWinner: function(contender1, contender2) {
            var c1 = contender1.specificity;
            var c2 = contender2.specificity;

            if (c1.a > c2.a) return contender1;
            if (c2.a > c1.a) return contender2;

            if (c1.b > c2.b) return contender1;
            if (c2.b > c1.b) return contender2;

            if (c1.c > c2.c) return contender1;
            if (c2.c > c1.c) return contender2;

            if (c1.d > c2.d) return contender1;
            if (c2.d > c1.d) return contender2;

            return contender1;
        },

        paintBackground: function(ctx, x, y, w, h) {
            if (x === undefined) x = 0;
            if (y === undefined) y = 0;
            if (w === undefined) w = this.bounds.width;
            if (h === undefined) h = this.bounds.height;

            if (this.cssValue("background-color")) {
                ctx.fillStyle = this.cssValue("background-color");
                ctx.fillRect(x, y, w, h);
            }

            if (this.cssValue("background-image")) {
                var img = this.cssValue("background-image");
                var repeat = this.cssValue("background-repeat");
                var position = this.cssValue("background-position");

                var imgs = th.getCommaDelimitedItems(img);
                var repeats = (repeat) ? repeat.split(",") : [ undefined ];
                var positions = (position) ? position.split(",") : [ undefined ];

                var iterations = Math.max(imgs.length, repeats.length, positions.length);
                for (var i = 0; i < iterations; i++) {
                    var ci = i;
                    if (i >= imgs.length) ci = (imgs.length % i) - 1;
                    var cimg = this.processImage(th.trim(imgs[ci]), w, h);

                    if (cimg) {
                        var ri = i;
                        if (i >= repeats.length) ri = (repeats.length % i) - 1;

                        var pi = i;
                        if (i >= positions.length) pi = (positions.length % i) - 1;

                        this.paintImage(ctx, cimg, th.trim(repeats[ri]), th.trim(positions[pi]), x, y, w, h);
                    }
                }
            }
        },


        processImage: function(img, w, h) {
            if (img.indexOf("-webkit-gradient") == 0) {

                var parmstring = img.substring(img.indexOf("(") + 1, img.length - 1);

                var parms = th.getCommaDelimitedItems(parmstring);
                for (var i = 0; i < parms.length; i++) parms[i] = th.trim(parms[i]);

                if (parms[0] != "linear") {
                    console.log("Unsupported gradient: \"" + img + "\"; only linear gradients supported");
                    return undefined;
                }

                var pxy0 = parms[1].split(" ");
                var pxy1 = parms[2].split(" ");

                var xy0 = [];
                var xy1 = [];

                xy0[0] = (pxy0[0] == "top" || pxy0[0] == "bottom") ? pxy0[1] : pxy0[0];
                xy0[1] = (pxy0[0] == "top" || pxy0[0] == "bottom") ? pxy0[0] : pxy0[1];
                xy1[0] = (pxy1[0] == "top" || pxy1[0] == "bottom") ? pxy1[1] : pxy1[0];
                xy1[1] = (pxy1[0] == "top" || pxy1[0] == "bottom") ? pxy1[0] : pxy1[1];

                var bothpoints = [ xy0, xy1 ];
                for (var a = 0; a < bothpoints.length; a++) {
                    var pa = bothpoints[a];
                    for (var b = 0; b < pa.length; b++) {
                        if (pa[b] == "top") pa[b] = "0%";
                        if (pa[b] == "bottom") pa[b] = "100%";
                        if (pa[b] == "left") pa[b] = "0%";
                        if (pa[b] == "right") pa[b] = "100%";

                        if (th.isPercentage(pa[b])) {
                            var multiplier = parseInt(pa[b]) / 100;
                            pa[b] = multiplier * ( (b == 0) ? w : h );
                        }
                    }
                }

                var canvas = document.createElement("canvas");
                canvas.setAttribute("width", w);
                canvas.setAttribute("height", h);
                var ctx = canvas.getContext("2d");

                var gradient = ctx.createLinearGradient(xy0[0], xy0[1], xy1[0], xy1[1]);

                for (var c = 3; c < parms.length; c++) {
                    var vals = parms[c].split("(");

                    if (vals[1].indexOf(",") != -1) {
                        var posColor = vals[1].split(",");
                        gradient.addColorStop(posColor[0], posColor[1]);
                    } else {
                        vals[1] = vals[1].substring(0, vals[1].length - 1); // kill the trailing right paren; this is now a color value

                        gradient.addColorStop((vals[0].toLowerCase() == "from" ? 0 : 1.0), vals[1]);
                    }
                }

                ctx.fillStyle = gradient;
                ctx.fillRect(0, 0, w, h);
                return canvas;
            } else {
                if (!th.global_resources.images[img]) {
                    console.log("Warning: image identified by '" + img + "'");
                } else {
                    return th.global_resources.images[img];
                }
            }
        },

        paintImage: function(ctx, img, repeat, position, x, y, w, h) {
            if (!repeat) repeat = "repeat";
            if (!position) position = "0% 0%";

            if (!x) x = 0;
            if (!y) y = 0;
            if (!w) w = this.bounds.width;
            if (!h) h = this.bounds.height;

            ctx.save();
            try {
                if ((x != 0) || (y != 0)) ctx.translate(x, y);

                var pos = position.toLowerCase().split(" ");
                if (pos.length == 1) pos.push("50%");
                if (pos.length != 2) {
                    console.log("Unsupported position syntax; only \"X\" or \"X Y\" supported, you passed in \" + position + \"");
                    return;
                }

                var xy = [];
                xy[0] = (pos[0] == "top" || pos[0] == "bottom") ? pos[1] : pos[0];
                xy[1] = (pos[0] == "top" || pos[0] == "bottom") ? pos[0] : pos[1];

                th.forEach(xy, function(p, index) {
                    if (p == "top") xy[index] = "0%";
                    if (p == "right") xy[index] = "100%";
                    if (p == "bottom") xy[index] = "100%";
                    if (p == "left") xy[index] = "0%";
                    if (p == "center") xy[index] = "50%";
                });

                var txy = [0, 0];
                for (var i = 0; i < xy.length; i++) {
                    var percentage = th.isPercentage(xy[i]);
                    var pixelPosition = this.convertPositionToPixel(xy[i], (i == 0) ? w : h);
                    if (percentage) txy[i] = this.convertPositionToPixel(xy[i], (i == 0) ? img.width : img.height);
                    xy[i] = pixelPosition;
                }

                var sx = xy[0] - txy[0];
                var sy = xy[1] - txy[1];

                if (!this.shouldRepeat(repeat)) {
                    ctx.drawImage(img, xy[0] - txy[0], xy[1] - txy[1]);
                } else {
                    var xloopEnd = (this.shouldRepeatX(repeat)) ? parseInt(w / img.width) + 1 : 1;
                    var yloopEnd = (this.shouldRepeatY(repeat)) ? parseInt(h / img.height) + 1 : 1;
                    if (this.shouldRepeatX(repeat)) while (sx > 0) sx -= img.width;
                    if (this.shouldRepeatY(repeat)) while (sy > 0) sy -= img.height;
                    var sxstart = sx;
                    for (var yloop = 0; yloop < yloopEnd; yloop++) {
                        sx = sxstart;
                        for (var xloop = 0; xloop < xloopEnd; xloop++) {
                            ctx.drawImage(img, sx, sy);
                            sx += img.width;
                        }
                        sy += img.height;
                    }
                }
            } finally {
                ctx.restore();
            }
        },

        shouldRepeatX: function(repeat) {
            return (repeat == "repeat" || repeat == "repeat-x");
        },

        shouldRepeatY: function(repeat) {
            return (repeat == "repeat" || repeat == "repeat-y");
        },

        shouldRepeat: function(repeat) {
            return (repeat != "no-repeat");
        },

        convertPositionToPixel: function(pos, totalLength) {
            if (th.isPercentage(pos)) {
                var per = pos.substring(0, pos.length - 1) / 100;
                return totalLength * per;
            } else if (th.isCssPixel(pos)) {
                return pos.substring(0, pos.length - 2);
            }
        },

        calculateInsets: function(cssPrefix, cssSuffix, insets) {
            if (!this["insetCache"]) this.insetCache = {};

            var cacheKey = cssPrefix + cssSuffix;
            if (this.insetCache[cacheKey]) return this.insetCache[cacheKey];

            if (!insets) insets = { top: 0, bottom: 0, left: 0, right: 0 };

            var component = (this.component) ? this.component : this;

            for (var side in insets) {
                var value = th.safeget(component.cssValue(cssPrefix + side + cssSuffix), 0);
                if (value) value = th.convertLengthToPixels(value, component);
                insets[side] += value;
            }

            this.insetCache[cacheKey] = insets;

            return insets;
        },

        getCurrentPseudoClass: function() {
            if (this.pseudoClass) return this.pseudoClass;
            return "(default)";
        },

        cssValue: function(property, pseudoBit) {
            if (pseudoBit === undefined) pseudoBit = this.getCurrentPseudoClass();

            if (this.localStyles[pseudoBit] === undefined) this.localStyles[pseudoBit] = {};
            if (this.localStyles[pseudoBit][property] !== undefined) return this.localStyles[pseudoBit][property];

            if (typeof this.styles == "undefined" || this.refreshCss) this.resolveCss();

            if (this.styles[pseudoBit] === undefined) this.styles[pseudoBit] = {};
            if (this.styles[pseudoBit][property]) return this.styles[pseudoBit][property];

            if (pseudoBit != "(default)") return this.cssValue(property, "(default)");

            if (property == "border-top-color" || property == "border-left-color" || property == "border-right-color" || property == "border-bottom-color") {
                return this.cssValue("color");
            }

            if (property == "border" || property == "border-top" || property == "border-left" || property == "border-right" || property == "border-bottom") {
                throw "Unsupported: request the individual border properties, please";
            }

            if (property == "margin") {
                throw "Unsupported: request the individual margin-top, margin-bottom, margin-right, and margin-left properties, please";
            }

            if (property == "padding") {
                throw "Unsupported: request the individual padding-top, padding-bottom, padding-right, and padding-left properties, please";
            }

            if ([ "color", "letter-spacing", "line-height", "text-align", "text-indent", "text-transform",
                    "visibility", "white-space", "word-spacing", "font" ].indexOf(property) != -1) {
                if (this.parent) return this.parent.cssValue(property);
            }
            if (property.indexOf("list-style-") != -1) if (this.parent) return this.parent.cssValue(property);

            return;
        },

        addCss: function(propertyOrHashOfProperties, value, pseudoBit) {
            if (th.isString(propertyOrHashOfProperties)) {
                var prop = propertyOrHashOfProperties;

                if (prop.indexOf("border") != -1 || prop.indexOf("padding") != -1 || prop.indexOf("margin") != -1) {
                    this.insetsCache = {};
                    this.border.insetsCache = {};
                }

                if (pseudoBit === undefined) pseudoBit = "(default)";
                if (this.localStyles[pseudoBit] === undefined) this.localStyles[pseudoBit] = {};
                this.localStyles[pseudoBit][prop] = value;
            } else {
                for (var property in propertyOrHashOfProperties) {
                    this.addCss(property, propertyOrHashOfProperties[property], pseudoBit);
                }
            }
        },

        getSize: function() {
            var bounds = this.bounds;
            return { width: bounds.width, height: bounds.height };
        },

        setBounds: function(x, y, w, h) {
            this.bounds = { x: x, y: y, width: w, height: h };
        },

        getPreferredSize: function() {
            if (this.getPreferredHeight && this.getPreferredWidth) {
                var height = this.getPreferredHeight();
                var width = this.getPreferredWidth();
                return { height: height, width: width };
            }

            var insets = this.getInsets();

            return { height: insets.top + insets.bottom, width: insets.left + insets.right };
        },

        getMinimumSize: function() {
            if (this.getMinimumHeight && this.getMinimumWidth) {
                var height = this.getMinimumHeight();
                var width = this.getMinimumWidth();
                return { height: height, width: width };
            }

            return this.getPreferredSize();
        },

        getMaximumSize: function() {
            if (this.getMaximumHeight && this.getMaximumWidth) {
                var height = this.getMaximumHeight();
                var width = this.getMaximumWidth();
                return { height: height, width: width };
            }

            return this.getPreferredSize();
        },

        getScene: function() {
            if (this.scene) return this.scene;

            if (!this.parent) return;

            var container = this.parent;
            while (!container.scene && container.parent) container = container.parent;
            return container.scene;
        }
    }
});

th.ContainerHelpers = new Trait({
    methods: {
        getChildById: function(id) {
            for (var i = 0; i < this.children.length; i++) {
                if (this.children[i].id == id) return this.children[i];
            }
        },

        getComponentForPosition: function(x, y, recurse) {
            for (var i = 0; i < this.children.length; i++) {
                if (!this.children[i].bounds) continue;

                if (this.children[i].bounds.x <= x && this.children[i].bounds.y <= y
                        && (this.children[i].bounds.x + this.children[i].bounds.width) >= x
                        && (this.children[i].bounds.y + this.children[i].bounds.height) >= y) {
                    if (!recurse) return this.children[i];

                    if (!this.children[i].shouldPaint()) continue;

                    return (this.children[i].getComponentForPosition) ?
                           this.children[i].getComponentForPosition(x - this.children[i].bounds.x, y - this.children[i].bounds.y, recurse) :
                           this.children[i];
                }
            }
            return this;
        },

        removeAll: function() {
            this.remove(this.children);
        },

        getMinimumSize: function() {
            return (this.layoutManager) ? this.layoutManager.getMinimumSize(this) : this._super();
        },

        getPreferredSize: function() {
            return (this.layoutManager) ? this.layoutManager.getPreferredSize(this) : this._super();
        },

        getMaximumSize: function() {
            return (this.layoutManager) ? this.layoutManager.getMaximumSize(this) : this._super();
        }
    }
});

if (typeof th == "undefined") th = {};

/*
 * Constants
 */
th.VERTICAL = "v";
th.HORIZONTAL = "h";

th.Resources = Class.define({
    members: {
        init: function() {
            this.loading = false;

            this.userAgentCss = [];
            this.authorCss = [];
            this.userCss = [];

            this.resolvedCssCache = {};

            this.blockUntilImagesLoaded = true;

            this.images = {};

            this.sheetCount = 0;
            this.currentSheet = 0;

            this.imageCount = 0;
            this.currentImage = 0;

            this.loaded = false;    // are all the resources loaded, including CSS and other stuff?
            this.cssLoaded = false; // are all the CSS sheets loaded?
            this.imagesLoaded = false;  // are all the images references by the CSS loaded?

            this.callbacks = [];
        },

        load: function(baseUrl, stringStyles) {
            if (this.loaded) return;    // no re-loading

            this.baseUrl = baseUrl;
            this.stringStyles = stringStyles;
            this.loading = true;
            this.parseCSS();
        },

        processImage: function() {
            this.currentImage++;
            if (this.imageCount == this.currentImage) {
                this.imagesLoaded = true;
                this.onLoaded();
            }
        },

        onLoaded: function() {
            if (this.cssLoaded && ((this.blockUntilImagesLoaded && this.imagesLoaded) || !this.blockUntilImagesLoaded)) {
                this.loaded = true;
                this.loading = false;
                if (this.callbacks) {
                    th.forEach(this.callbacks, function(item) {
                        if (item.context) {
                            item.callback.apply(item.context);
                        } else {
                            item.callback();
                        }
                    });
                }
            }
        },

        registerOnLoadCallback: function(callback, context) {
            this.callbacks.push({ callback: callback, context: context });
        },

        parseCSS: function() {
            var links = [];

            var usedStringifiedDefault = false;

            if (typeof this.baseUrl !== "undefined") {
                links.push({ url: this.baseUrl + "css/default.css", array: this.userAgentCss, index: 0 });
            } else if (th.DEFAULT_CSS) {
                this.processCSS(th.DEFAULT_CSS, this.userAgentCss, 0);
                usedStringifiedDefault = true;
            }

            var s, l = document.getElementsByTagName('link');
            var counter = 0;
            for (var i=0; i < l.length; i++){
                s = l[i];
                if (s.rel.toLowerCase().indexOf('thstylesheet') >= 0 && s.href) {
                    links.push({ url: s.href, array: this.authorCss, index: counter++ });
                }
            }

            if (this.stringStyles) {
                this.processCSS(this.stringStyles, this.authorCss, counter++);
            }

            if (links.length == 0) {
                this.cssLoaded = true;
                return this.onLoaded();
            }

            this.sheetCount = links.length;
            if (usedStringifiedDefault) this.sheetCount++;
            if (this.stringStyles) this.sheetCount++;

            th.forEach(links, function(link) {
                th.xhrGet({
                    url: link.url,
                    load: function(response) {
                        this.processCSS(response, link.array, link.index);
                    },
                    context: this
                });
            }, this);
        },

        processCSS: function(stylesheet, array, index) {
            array[index] = new th.CssParser().parse(stylesheet);

            for (var rule in array[index]) {

                var properties = [];
                for (var property in array[index][rule]) {
                    properties.push(property);
                }

                for (var i = 0; i < properties.length; i++) {
                    property = properties[i];
                    var value = array[index][rule][property];

                    var imageSearchPos = 0;
                    var urlFoundPos = -1;
                    while ((urlFoundPos = value.indexOf("url(", imageSearchPos)) != -1) {
                        var endOfUrlPos = value.indexOf(")", urlFoundPos);
                        if (endOfUrlPos == -1) {
                            console.log("Warning: malformed url found ('" + value + "')");
                            break;
                        }

                        var cacheUrl = value.substring(urlFoundPos, endOfUrlPos);
                        var url = cacheUrl.substring(4);

                        if (url.charAt(0) == "'" || url.charAt(0) == "\"") {
                            url = url.substring(1, url.length - 1);
                        }

                        this.imageCount++;
                        var image = new Image();

                        if (this.blockUntilImagesLoaded) {
                            this.imagesLoaded = false;
                            var self = this;
                            image.onload = function() {
                                self.processImage();
                            }
                            image.onerror = function() {
                                self.processImage();
                            }
                        }

                        image.src = url;
                        this.images[value] = image;

                        imageSearchPos = endOfUrlPos + 1;
                    }

                    if (property == "margin" || property == "padding") {
                        this.expandMarginOrPadding(property, value, array[index][rule], properties);
                    }

                    if (property == "border") {
                        this.expandProperty("border-top", value, array[index][rule], properties);
                        this.expandProperty("border-right", value, array[index][rule], properties);
                        this.expandProperty("border-bottom", value, array[index][rule], properties);
                        this.expandProperty("border-left", value, array[index][rule], properties);
                    }

                    if (property == "border-top" || property == "border-left" || property == "border-right" || property == "border-bottom") {
                        this.expandBorder(property, value, array[index][rule], properties);
                    }
                }
            }

            if (++this.currentSheet == this.sheetCount) {
                this.cssLoaded = true;
                this.onLoaded();
            }
        },

        expandBorder: function(property, value, css, propertyList) {
            var values = th.getSpaceDelimitedItems(value);
            for (var i = 0; i < values.length; i++) {
                if (th.isCssLength(values[i])) {
                    this.expandProperty(property + "-width", values[i], css, propertyList);
                } else if (th.isCssBorderStyle(values[i])) {
                    this.expandProperty(property + "-style", values[i], css, propertyList);
                } else {
                    this.expandProperty(property + "-color", values[i], css, propertyList);
                }
            }
            delete css[property];
        },

        expandProperty: function(newProperty, value, css, propertyList) {
            css[newProperty] = value;
            propertyList.push(newProperty);
        },

        expandMarginOrPadding: function(property, value, css, propertyList) {
            var values = value.split(" ");
            if (values.length == 3) values = [ values[0], values[1], values[2], values[1] ];
            if (values.length == 2) values = [ values[0], values[1], values[0], values[1] ];
            if (values.length == 1) values = [ values[0], values[0], values[0], values[0] ];

            if (values.length >= 4) {  // top right bottom left
                this.expandProperty(property + "-top", values[0], css, propertyList);
                this.expandProperty(property + "-right", values[1], css, propertyList);
                this.expandProperty(property + "-bottom", values[2], css, propertyList);
                this.expandProperty(property + "-left", values[3], css, propertyList);

                delete css[property];
            }
        }
    }
});

/*
    Event bus; all listeners and events pass through a single global instance of this class.
 */
th.Bus = Class.define({
    members: {
        init: function() {
            this.events = {};
        },


        bind: function(event, selector, listenerFn, listenerContext) {
            var listeners = this.events[event];
            if (!listeners) {
                listeners = [];
                this.events[event] = listeners;
            }
            selector = th.isArray(selector) ? selector : [ selector ];
            for (var z = 0; z < selector.length; z++) {
                for (var i = 0; i < listeners.length; i++) {
                    if (listeners[i].selector == selector[z] && listeners[i].listenerFn == listenerFn) return;
                }
                listeners.push({ selector: selector[z], listenerFn: listenerFn, context: listenerContext });
            }
        },

        unbind: function(selector) {
            for (var event in this.events) {
                var listeners = this.events[event];

                for (var i = 0; i < listeners.length; i++) {
                    if (listeners[i].selector === selector) {
                        this.events[event] = th.remove(listeners, listeners[i]);
                        listeners = this.events[event];
                        i--;
                    }
                }
            }
        },

        fire: function(eventName, eventDetails, component) {
            var listeners = this.events[eventName];
            if (!listeners || listeners.length == 0) return;

            for (var i = 0; i < listeners.length; i++) {
                if (listeners[i].selector.constructor == String) {
                    if (listeners[i].selector.charAt(0) == "#") {
                        if (component.id == listeners[i].selector.substring(1)) {
                            this.dispatchEvent(eventName, eventDetails, component, listeners[i]);
                        }
                    } else if (listeners[i].selector == component.declaredClass) {
                        this.dispatchEvent(eventName, eventDetails, component, listeners[i]);
                    }
                } else if (listeners[i].selector == component) {
                    this.dispatchEvent(eventName, eventDetails, component, listeners[i]);
                }
            }
        },

        dispatchEvent: function(eventName, eventDetails, component, listener) {
            eventDetails.thComponent = component;

            if (listener.context) {
                listener.listenerFn.apply(listener.context, [ eventDetails ]);
            } else {
                listener.listenerFn(eventDetails);
            }
        }
    }
});

th.global_event_bus = new th.Bus();

th.global_resources = new th.Resources();

th.Scene = Class.define({
    uses: [
        th.EventHelpers
    ],

    members: {
        bus: th.global_event_bus,

        init: function(canvasOrId, baseUrlOrParams) {
            if (th.isString(canvasOrId)) {
                canvasOrId = document.getElementById("canvasOrId");
            }
            this.canvas = canvasOrId;

            var baseUrl = baseUrlOrParams;
            var stringStyles = "";
            if (baseUrlOrParams && !th.isString(baseUrlOrParams)) {
                baseUrl = baseUrlOrParams.baseUrl;
                stringStyles = baseUrlOrParams.stringStyles;
            }

            this.smartRedraw = false;

            this.resources = th.global_resources;

            this.resourceCallbackRegistered = false;

            if (!this.resources.loaded && !this.resources.loading) this.resources.load(baseUrl, stringStyles);

            th.observe(window, "resize", function() {
                this.render();
            }, this);

            this.root = new th.Panel({ id: "root" });
            this.root.scene = this;

            var testCanvas = document.createElement("canvas");
            this.scratchContext = testCanvas.getContext("2d");
            th.fixCanvas(this.scratchContext);

            this.parseTags();

            th.observe(window, "mousedown", function(e) {
                this.wrapEvent(e, this.root);

                this.mouseDownComponent = e.thComponent;

                th.global_event_bus.fire("mousedown", e, e.thComponent);
            }, this);

            th.observe(window, "dblclick", function(e) {
                this.wrapEvent(e, this.root);

                th.global_event_bus.fire("dblclick", e, e.thComponent);
            }, this);

            th.observe(window, "click", function(e) {
                this.wrapEvent(e, this.root);

                th.global_event_bus.fire("click", e, e.thComponent);
            }, this);

            th.observe(window, "mousemove", function(e) {
                this.wrapEvent(e, this.root);

                th.global_event_bus.fire("mousemove", e, e.thComponent);

                if (this.mouseDownComponent) {
                    this.addComponentXY(e, this.root, this.mouseDownComponent);
                    th.global_event_bus.fire("mousedrag", e, this.mouseDownComponent);
                }
            }, this);

            th.observe(window, "mouseup", function(e) {
                if (!this.mouseDownComponent) return;

                this.addComponentXY(e, this.root, this.mouseDownComponent);
                th.global_event_bus.fire("mouseup", e, this.mouseDownComponent);

                delete this.mouseDownComponent;
            }, this);
        },

        render: function(callback) {
            if (!this.resources.loaded) {
                if (!this.resourceCallbackRegistered) {
                    this.resources.registerOnLoadCallback(this.render, this);
                    if (callback) this.resources.registerOnLoadCallback(function() {
                        callback(this);
                    }, this);
                    this.resourceCallbackRegistered = true;
                }

                return;
            }

            this.layout();
            this.paint();

            if (callback) {
                callback(this);
            }
        },

        blockUntilRender: function() {
            if (!this.resources.loaded) {

            }
        },

        parseTags: function() {
            this.parseCanvasAttributes("grid");

            var children = this.canvas.childNodes;
            this.attachChildElementsToParent(children, this.root);
        },

        parseCanvasAttributes: function() {
            th.forEach(arguments, function(argument) {
                if (this.canvas.hasAttribute(argument)) {
                    this.root[argument] = this.canvas.getAttribute(argument);
                }
            }, this);
        },

        attachChildElementsToParent: function(children, parent) {
            th.forEach(children, function(child) {
                if (child.nodeType == Node.ELEMENT_NODE) {
                    var component = this.createComponentFromElement(child, parent);
                    if (component) {
                        parent.add(component);
                        this.attachChildElementsToParent(child.childNodes, component);
                    } else {
                        console.log("Couldn't create component from element \"" + child.tagName + "\"");
                    }
                }
            }, this);
        },

        createComponentFromElement: function(child, futureParent) {
            var componentMap = {};
            for (var fieldName in th) {
                var lower = fieldName.toLowerCase();
                if (componentMap[lower]) console.log("componentMap collision with key \"" + lower + "\"");
                componentMap[lower] = fieldName;
            }

            var tagName = child.tagName.toLowerCase();

            fieldName = componentMap[tagName];

            if (!fieldName) return;            // no field that matches the tag name

            var constructor = th[fieldName];
            if (!constructor) return;          // field no longer exists in th; this shouldn't ever happen

            var attr = child.attributes;
            var params = {};
            th.forEach(attr, function(attribute) {
                var name = attribute.name;
                var value = attribute.value;

                if (name == "cell") name = "constraints";

                params[name] = value;
            });

            var potentialComponent = new constructor(params);

            if (!potentialComponent.type) return;       // created instance doesn't have type field and therefore is not a component
            if (potentialComponent.type.toLowerCase() != tagName) return;       // type name doesn't match tag name

            return potentialComponent;
        },

        layout: function() {
            if (this.root) {
                this.root.bounds = { x: 0, y: 0, width: this.canvas.width, height: this.canvas.height };
                this.root.layoutTree();
            }
        },

        paint: function(component) {
            if (!this.resources.loaded) {
                if (!this.resourceCallbackRegistered) {
                    this.resources.registerOnLoadCallback(this.render, this);
                    this.resourceCallbackRegistered = true;
                }

                return;
            }

            if (!component) component = this.root;

            if (component) {
                if (component === this.root) component.resolveCss();

                if (!component.opaque && component.parent) {
                    return this.paint(component.parent);
                }

                var ctx = this.canvas.getContext("2d");
                th.fixCanvas(ctx);

                ctx.save();

                var parent = component.parent;
                var child = component;
                while (parent) {
                    ctx.translate(child.bounds.x, child.bounds.y);
                    child = parent;
                    parent = parent.parent;
                }

                if (!this.smartRedraw) {
                    ctx.clearRect(0, 0, component.bounds.width, component.bounds.height);
                }
                ctx.beginPath();
                ctx.rect(0, 0, component.bounds.width, component.bounds.height);
                ctx.closePath();
                ctx.clip();
                component.paint(ctx);

                ctx.restore();
            }
        }
    }
});

/*th.Rectangle = Class.define({
    members: {
        TOP: 0,     TOP_LEFT: 0,
        RIGHT: 1,   TOP_RIGHT: 1,
        BOTTOM: 2,  BOTTOM_RIGHT:2,
        LEFT: 3,    BOTTOM_LEFT: 3,

        init: function () {
            if (arguments.length == 1) {
                this.x = arguments[0].x;
                this.y = arguments[0].y;
                this.w = arguments[0].w;
                this.h = arguments[0].h;
            } else if (arguments.length == 4) {
                this.x = arguments[0];
                this.y = arguments[1];
                this.w = arguments[2];
                this.h = arguments[3];
            } else {
                this.x = this.y = this.w = this.h = 0;
            }
        },

        round: function () {
            this.x = Math.round(this.x);
            this.y = Math.round(this.y);
            this.w = Math.round(this.w);
            this.h = Math.round(this.h);
        },

        inset: function () {
            if (arguments.length == 1) {
                var d = arguments[0];
                if (typeof d == "number") {
                    this.x +=d;
                    this.y +=d;
                    this.w -= 2*d;
                    this.h -= 2*d;
                } else if ((d instanceof Array) && d.length == 4) {
                    this.x += d[this.LEFT];
                    this.y += d[this.TOP];
                    this.w -= d[this.RIGHT] + d[this.LEFT];
                    this.h -= d[this.TOP] + d[this.BOTTOM];
                }
            } else if (arguments.length == 4) {
                var top = arguments[0];
                var right = arguments[1];
                var bottom = arguments[2];
                var left = arguments[3];

                this.x += left;
                this.y += top;
                this.w -= left + right;
                this.h -= top + bottom;
            }
        },

        corner: function (corner) {
            switch (corner) {
                case this.TOP_LEFT:
                    return {x: this.x, y: this.y};
                case this.TOP_RIGHT:
                    return {x: this.x + this.w, y: this.y};
                case this.BOTTOM_RIGHT:
                    return {x: this.x + this.w, y: this.y + this.h};
                case this.BOTTOM_LEFT:
                    return {x: this.x, y: this.y + this.h};
            }
        },

        isEmpty: function () {
            return (this.w <= 0 || this.h <= 0);
        }
    }
});*/

th.Rectangle = function () {
    if (arguments.length == 1) {
        this.x = arguments[0].x;
        this.y = arguments[0].y;
        this.w = arguments[0].w;
        this.h = arguments[0].h;
    } else if (arguments.length == 4) {
        this.x = arguments[0];
        this.y = arguments[1];
        this.w = arguments[2];
        this.h = arguments[3];
    } else {
        this.x = this.y = this.w = this.h = 0;
    }
};

th.Rectangle.prototype.TOP = 0;
th.Rectangle.prototype.RIGHT = 1;
th.Rectangle.prototype.BOTTOM = 2;
th.Rectangle.prototype.LEFT = 3;
th.Rectangle.prototype.TOP_LEFT = 0;
th.Rectangle.prototype.TOP_RIGHT = 1;
th.Rectangle.prototype.BOTTOM_RIGHT = 2;
th.Rectangle.prototype.BOTTOM_LEFT = 3;

th.Rectangle.prototype.round = function () {
    this.x = Math.round(this.x);
    this.y = Math.round(this.y);
    this.w = Math.round(this.w);
    this.h = Math.round(this.h);
};

th.Rectangle.prototype.inset = function () {
    if (arguments.length == 1) {
        var d = arguments[0];
        if (typeof d == "number") {
            this.x +=d;
            this.y +=d;
            this.w -= 2*d;
            this.h -= 2*d;
        } else if ((d instanceof Array) && d.length == 4) {
            this.x += d[this.LEFT];
            this.y += d[this.TOP];
            this.w -= d[this.RIGHT] + d[this.LEFT];
            this.h -= d[this.TOP] + d[this.BOTTOM];
        }
    } else if (arguments.length == 4) {
        var top = arguments[0];
        var right = arguments[1];
        var bottom = arguments[2];
        var left = arguments[3];

        this.x += left;
        this.y += top;
        this.w -= left + right;
        this.h -= top + bottom;
    }
};

th.Rectangle.prototype.corner = function (corner) {
    switch (corner) {
        case this.TOP_LEFT:
            return {x: this.x, y: this.y};
        case this.TOP_RIGHT:
            return {x: this.x + this.w, y: this.y};
        case this.BOTTOM_RIGHT:
            return {x: this.x + this.w, y: this.y + this.h};
        case this.BOTTOM_LEFT:
            return {x: this.x, y: this.y + this.h};
    }
};

th.Rectangle.prototype.isEmpty = function () {
    return (this.w <= 0 || this.h <= 0);
};

/*th.Size2D = Class.define({
    type: "Size2D",
    members: {
        init: function () {
            if (arguments.length == 1) {
                this.w = arguments[0].w;
                this.h = arguments[0].h;
            } else if (arguments.length == 2) {
                this.w = arguments[0];
                this.h = arguments[1];
            } else {
                this.w = this.h = 0;
            }
        },

        isZero: function () {
            return (this.w == 0) && (this.h == 0);
        }
    }
});*/

th.Size2D = function () {
    if (arguments.length == 1) {
        this.w = arguments[0].w;
        this.h = arguments[0].h;
    } else if (arguments.length == 2) {
        this.w = arguments[0];
        this.h = arguments[1];
    } else {
        this.w = this.h = 0;
    }
};

th.Size2D.prototype.isZero = function () {
    return (this.w == 0) && (this.h == 0);
};

th.SimpleBorder = Class.define({
    type: "Border",

    uses: [
        th.ComponentHelpers
    ],

    members: {
        init: function (component) {
            this.SIDES = ["top", "right", "bottom", "left"];

            this.component = component;
        },

        getBorderWidth: function(side) {
            var length = this.component.cssValue("border-" + side + "-width");
            if (length === undefined) return 0; // TODO: should be "medium" by default
            return th.convertLengthToPixels(length, this.component);
        },

        getInsets: function() {
            return this.calculateInsets();
        },

        paint: function(ctx) {
            for (var i = 0; i < this.SIDES.length; i++) {
                var s = this.SIDES[i];
                var width = this.getBorderWidth(s);
                if (width > 0) {
                    var color = this.component.cssValue("border-" + s + "-color");

                    var x, y, h, w;

                    switch (s) {
                        case "top":
                            x = 0;
                            y = 0;
                            h = width;
                            w = this.component.bounds.width;
                            break;
                        case "right":
                            x = this.component.bounds.width - width;
                            y = 0;
                            h = this.component.bounds.height;
                            w = width;
                            break;
                        case "bottom":
                            x = 0;
                            y = this.component.bounds.height - width;
                            h = width;
                            w = this.component.bounds.width;
                            break;
                        case "left":
                            x = 0;
                            y = 0;
                            h = this.component.bounds.height;
                            w = width;
                            break;
                    }

                    this.paintBorder(ctx, color, x, y, w, h);
                }
            }
        },

        paintBorder: function(ctx, color, x, y, w, h) {
            ctx.fillStyle = color;
            ctx.fillRect(x, y, w, h);
        }
    }
});

th.Border = Class.define({
    type: "Border",

    uses: [
        th.ComponentHelpers
    ],

    members: {
        SIDES: ["top", "right", "bottom", "left"],
        CORNERS: ["top-left", "top-right", "bottom-right", "bottom-left"],

        TOP: 0,     TOP_LEFT: 0,
        RIGHT: 1,   TOP_RIGHT: 1,
        BOTTOM: 2,  BOTTOM_RIGHT:2,
        LEFT: 3,    BOTTOM_LEFT: 3,

        BIT_TOP: 1,
        BIT_RIGHT: 2,
        BIT_BOTTOM: 4,
        BIT_LEFT: 8,

        init: function (component) {
            this.component = component;

            this.outerRect = new th.Rectangle();
            this.innerRect = new th.Rectangle();

            this.borderStyles = [];
            this.borderWidths = [];
            this.borderRadii = [];
            this.borderColors = [];
            this.skipSides = 0;
            this.oneUnitBorder = false;
            this.noBorderRadius = false;
            this.cornerDimensions = {};

            for (var i = 0; i < 4; i++) {
                this.borderStyles[i] = "none";
                this.borderWidths[i] = 0;
                this.borderColors[i] = "rgba(0, 0, 0, 0)";
                this.borderRadii[i] = new th.Size2D();
                this.cornerDimensions[i] = new th.Size2D();
            }
        },

        getInsets: function() {
            return this.calculateInsets("border-", "-width");
        },

        recalculateBorderData: function () {
            var d = this.component.d();
            var tmpctx = this.component.getScratchContext(); // this is for normalizing the colors
            var borderSideColor = '', x1, x2, x3;
            for (var i = 0; i < 4; i++) {
                var side = this.SIDES[i];
                x1 = th.convertBorderLengthToPixels(this.component.cssValue("border-" + side + "-width"));
                borderSideColor = this.component.cssValue("border-" + side + "-color");
                if (borderSideColor !== undefined) {
                    tmpctx.fillStyle = borderSideColor;
                    x2 = tmpctx.fillStyle;
                    x3 = this.component.cssValue("border-" + side + "-style");
                }
                this.borderWidths[i] = (x1 === undefined) ? 0 : x1;
                this.borderColors[i] = (x2 === undefined) ? "rgb(0, 0, 0)" : x2;
                this.borderStyles[i] = (x3 === undefined) ? "none" : x3;
            }

            this.outerRect = new th.Rectangle(0, 0, d.b.w, d.b.h);
            this.innerRect = new th.Rectangle(this.outerRect);
            this.innerRect.inset(this.borderWidths[0], this.borderWidths[1], this.borderWidths[2], this.borderWidths[3]);

            this.cornerDimensions[0].w = this.borderWidths[3];
            this.cornerDimensions[0].h = this.borderWidths[0];
            this.cornerDimensions[1].w = this.borderWidths[1];
            this.cornerDimensions[1].h = this.borderWidths[0];
            this.cornerDimensions[2].w = this.borderWidths[1];
            this.cornerDimensions[2].h = this.borderWidths[2];
            this.cornerDimensions[3].w = this.borderWidths[3];
            this.cornerDimensions[3].h = this.borderWidths[2];

            this.oneUnitBorder = this.checkFourFloatsEqual(this.borderWidths, 1);
            this.noBorderRadius = this.allCornersZeroSize(this.borderRadii);
            borderSideColor = this.component.cssValue("background-color");
            if (borderSideColor !== undefined) {
                tmpctx.fillStyle = borderSideColor;
            }
            this.backgroundColor = tmpctx.fillStyle;
        },

        next: function (s) {
            var i = (s + 1) % 4;
            if (i < 0) return i + 4;
            return i;
        },

        prev: function (s) {
            var i = (s - 1) % 4;
            if (i < 0) return i + 4;
            return i;
        },

        allCornersZeroSize: function (arr) {
            return (arr[0].isZero() && arr[1].isZero() && arr[2].isZero() && arr[3].isZero());
        },

        checkFourFloatsEqual: function (arr, f) { // this works for more than just floats :)
            return (arr[0] == f && arr[1] == f && arr[2] == f && arr[3] == f);
        },

        areBorderSideFinalStylesSame: function (aSides) {
            if (aSides == 0) throw "th.Border:areBorderSideFinalStyleSame - Invalid side bit-field '0'";
            firstStyle = 0;
            for (var i = 0; i < 4; i++) {
                if (firstStyle == i) {
                    if (((1 << i) & aSides) == 0)
                        firstStyle++;
                    continue;
                }
                if (((1 << i) & aSides) == 0)
                    continue;
                if (this.borderStyles[firstStyle] != this.borderStyles[i] ||
                    this.borderColors[firstStyle] != this.borderColors[i])
                    return false;
            }

            switch(this.borderStyles[firstStyle]) {
                case "groove":
                case "ridge":
                case "inset":
                case "outset":
                    return ((aSides & (this.BIT_TOP | this.BIT_LEFT)) == 0 ||
                            (aSides & (this.BIT_BOTTOM | this.BIT_RIGHT)) == 0)
            }

            return true;
        },

        isSolidCornerStyle: function (style, aCorner) {
            switch(style) {
                case "dotted":
                case "dashed":
                case "solid":
                    return true;
                case "inset":
                case "outset":
                    return aCorner == this.TOP_LEFT || aCorner == this.BOTTOM_RIGHT;
                case "groove":
                case "ridge":
                    return this.oneUnitBorder && (aCorner == this.TOP_LEFT || aCorner == this.BOTTOM_RIGHT);
                case "double":
                    return this.oneUnitBorder;
                default:
                    return false;
            }
        },

        borderColorStyleForSolidCorner: function (style, aCorner) {
            switch (style) {
                case "dotted":
                case "dashed":
                case "solid":
                case "double":
                    return "solid";
                case "inset":
                case "groove":
                    if (aCorner == this.TOP_LEFT)
                        return "dark";
                    else if (aCorner == this.BOTTOM_RIGHT)
                        return "light";
                    break;
                case "outset":
                case "ridge":
                    if (aCorner == this.TOP_LEFT)
                        return "light";
                    else if (aCorner == this.BOTTOM_RIGHT)
                        return "dark";
                    break;
            }
            return "none";
        },

        doCornerSubPath: function (ctx, aCorner) {
            var offset = {x:0, y:0};
            if (aCorner == this.TOP_RIGHT || aCorner == this.BOTTOM_RIGHT)
                offset.x = this.outerRect.w - this.cornerDimensions[aCorner].w;
            if (aCorner == this.BOTTOM_RIGHT || aCorner == this.BOTTOM_LEFT)
                offset.y = this.outerRect.h - this.cornerDimensions[aCorner].h;
            ctx.rect(this.outerRect.x + offset.x, this.outerRect.y + offset.y,
                    this.cornerDimensions[aCorner].w, this.cornerDimensions[aCorner].h);
        },

        doSideClipWithoutCornersSubPath: function (ctx, aSide) {
            var offset = {x: 0, y: 0};
            var nextSide = this.next(aSide);
            if (aSide == this.TOP) {
                offset.x = this.cornerDimensions[this.TOP_LEFT].w;
            } else if (aSide == this.RIGHT) {
                offset.x = this.outerRect.w - this.borderWidths[this.RIGHT];
                offset.y = this.cornerDimensions[this.TOP_RIGHT].h;
            } else if (aSide == this.BOTTOM) {
                offset.x = this.cornerDimensions[this.BOTTOM_LEFT].w;
                offset.y = this.outerRect.h - this.borderWidths[this.BOTTOM];
            } else if (aSide == this.LEFT) {
                offset.y = this.cornerDimensions[this.TOP_LEFT].h;
            }

            var sideCornerSum = new th.Size2D();
            sideCornerSum.w = this.cornerDimensions[aSide].w + this.cornerDimensions[nextSide].w;
            sideCornerSum.h = this.cornerDimensions[aSide].h + this.cornerDimensions[nextSide].h;

            var rect = new th.Rectangle(this.outerRect.x + offset.x, this.outerRect.y + offset.y,
                    this.outerRect.w - sideCornerSum.w, this.outerRect.h - sideCornerSum.h);

            if (aSide == this.TOP || aSide == this.BOTTOM)
                rect.h = this.borderWidths[aSide];
            else
                rect.w = this.borderWidths[aSide];

            ctx.rect(rect.x, rect.y, rect.w, rect.h);
        },

        maybeMoveToMidPoint: function (aP0, aP1, aMidPoint) {
            var ps = {x: aP1.x - aP0.x, y: aP1.y - aP0.y};

            if (ps.x != 0 && ps.y != 0) {
                k = Math.min((aMidPoint.x - aP0.x) / ps.x,
                        (aMidPoint.y - aP1.y) / ps.y);
                aP1.x = aP0.x + ps.x * k;
                aP1.y = aP0.y + px.y * k;
            }
        },

        doSideClipSubPath: function (ctx, aSide) {
            var start = [{x:0, y:0}, {x:0, y:0}];
            var end = [{x:0, y:0}, {x:0, y:0}];
            var prevSide = this.prev(aSide);
            var nextSide = this.next(aSide);

            var isDashed = this.borderStyles[aSide] == "dashed" || this.borderStyles[aSide] == "dotted";
            var startIsDashed = this.borderStyles[prevSide] == "dashed" || this.borderStyles[prevSide] == "dotted";;
            var endIsDashed = this.borderStyles[nextSide] == "dashed" || this.borderStyles[nextSide] == "dotted";;

            var startType = 0; // Trapezoid
            var endType = 0; // Trapezoid

            if (!this.borderRadii[aSide].isZero())
                startType = 1; // Trapezoid Full
            else if (startIsDashed && isDashed)
                startType = 2; // Rectangle

            if (!this.borderRadii[nextSide].isZero())
                endType = 1; // Trapezoid Full
            else if (startIsDashed && isDashed)
                endType = 2; // Rectangle

            var midPoint = {x:0, y:0};
            midPoint.x = this.innerRect.x + this.innerRect.w / 2.0;
            midPoint.y = this.innerRect.y + this.innerRect.h / 2.0;

            start[0] = this.outerRect.corner(aSide);
            start[1] = this.innerRect.corner(aSide);

            end[0] = this.outerRect.corner(nextSide);
            end[1] = this.innerRect.corner(nextSide);

            if (startType == 1) { // Trapezoid Full
                this.maybeMoveToMidPoint(start[0], start[1], midPoint);
            } else if (startType == 2) { // Rectangle
                if (side == "top" || side == "bottom")
                    start[1] = {x: this.outerRect.corner(aSide).x,
                        y: this.innerRect.corner(aSide).y};
                else
                    start[1] = {x: this.innerRect.corner(aSide).x,
                        y: this.outerRect.corner(aSide).y};
            }

            if (endType == 1) { // Trapezoid Full
                this.maybeMoveToMidPoint(end[0], end[1], midPoint);
            } else if (endType == 2) { // Rectangle
                if (side == "top" || side == "bottom")
                    end[0] = {x: this.innerRect.corner(nextSide).x,
                        y: this.outerRect.corner(nextSide).y};
                else
                    end[0] = {x: this.outerRect.corner(nextSide).x,
                        y: this.innerRect.corner(nextSide).y};
            }

            ctx.moveTo(start[0].x, start[0].y);
            ctx.lineTo(end[0].x, end[0].y);
            ctx.lineTo(end[1].x, end[1].y);
            ctx.lineTo(start[1].x, start[1].y);
            ctx.closePath();
        },

        fillSolidBorder: function (ctx, outerRect, innerRect, borderRadii, borderSizes, aSides, color) {
            ctx.fillStyle = color;
            if (!this.allCornersZeroSize(borderRadii)) {
                var innerRadii = this.computeInnerRadii(borderRadii, borderSizes);
                ctx.beginPath();
                this.traceRoundRect(ctx, outerRect, borderRadii, true);
                this.traceRoundRect(ctx, innerRect, innerRadii, false);
                ctx.fill();
                return;
            }

            if (aSides == 15 && this.checkFourFloatsEqual(borderSizes, borderSizes[0])) {
                var r = new th.Rectangle(outerRect);
                r.inset(borderSizes[0] / 2.0);
                ctx.strokeStyle = color;
                ctx.lineWidth = borderSizes[0];
                ctx.beginPath();
                ctx.rect(r.x, r.y, r.w, r.h);
                ctx.stroke();
                return;
            }

            var r = [
                {x:0, y:0, w:0, h:0},
                {x:0, y:0, w:0, h:0},
                {x:0, y:0, w:0, h:0},
                {x:0, y:0, w:0, h:0}
            ];

            if (aSides & this.BIT_TOP) {
                r[this.TOP].x = outerRect.x;
                r[this.TOP].y = outerRect.y;
                r[this.TOP].w = outerRect.w;
                r[this.TOP].h = borderSizes[this.TOP];
            }
            if (aSides & this.BIT_BOTTOM) {
                r[this.BOTTOM].x = outerRect.x;
                r[this.BOTTOM].y = outerRect.y + outerRect.h;
                r[this.BOTTOM].y -= borderSizes[this.BOTTOM];
                r[this.BOTTOM].w = outerRect.w;
                r[this.BOTTOM].h = borderSizes[this.BOTTOM];
            }
            if (aSides & this.BIT_LEFT) {
                r[this.LEFT].x = outerRect.x;
                r[this.LEFT].y = outerRect.y;
                r[this.LEFT].w = borderSizes[this.LEFT];
                r[this.LEFT].h = outerRect.h;
            }
            if (aSides & this.BIT_RIGHT) {
                r[this.RIGHT].x = outerRect.x + outerRect.w;
                r[this.RIGHT].y = outerRect.y;
                r[this.RIGHT].x -= borderSizes[this.RIGHT];
                r[this.RIGHT].w = borderSizes[this.RIGHT];
                r[this.RIGHT].h = outerRect.h;
            }

            if ((aSides & (this.BIT_TOP | this.BIT_LEFT)) == (this.BIT_TOP | this.BIT_LEFT)) {
                r[this.LEFT].y += borderSizes[this.TOP];
                r[this.LEFT].h -= borderSizes[this.TOP];
            }
            if ((aSides & (this.BIT_TOP | this.BIT_RIGHT)) == (this.BIT_TOP | this.BIT_RIGHT)) {
                r[this.TOP].w -= borderSizes[this.RIGHT];
            }
            if ((aSides & (this.BIT_BOTTOM | this.BIT_RIGHT)) == (this.BIT_BOTTOM | this.BIT_RIGHT)) {
                r[this.RIGHT].h -= borderSizes[this.BOTTOM];
            }
            if ((aSides & (this.BIT_BOTTOM | this.BIT_LEFT)) == (this.BIT_BOTTOM | this.BIT_LEFT)) {
                r[this.BOTTOM].x += borderSizes[this.LEFT];
                r[this.BOTTOM].w -= borderSizes[this.LEFT];
            }

            for (var i = 0; i < 4; i++) {
                if (aSides & (1 << i)) {
                    ctx.beginPath();
                    ctx.rect(r[i].x, r[i].y, r[i].w, r[i].h);
                    ctx.fill();
                }
            }
        },

        getLuminosity: function (r, g, b) {
            return r * 299 + g * 587 + b * 114;
        },

        getBrightness: function (r, g, b) {
            var intensity = (r + g + b) / 3;
            var luminosity = this.getLuminosity(r, g, b) / 1000;
            return (intensity * 25 + luminosity * 75) / 100;
        },

        getSpecial3DColors: function (aBackgroundColor, aColor) {
            var aResult = [];
            var f0, f1;
            var rb = parseInt(aColor.substring(1, 2), 16);
            var gb = parseInt(aColor.substring(3, 2), 16);
            var bb = parseInt(aColor.substring(5, 2), 16);
            var red = parseInt(aBackgroundColor.substring(1, 2), 16);
            var green = parseInt(aBackgroundColor.substring(3, 2), 16);
            var blue = parseInt(aBackgroundColor.substring(5, 2), 16);
            var elementBrightness = this.getBrightness(rb, gb, bb);
            var backgroundBrightness = this.getBrightness(red, green, blue);
            if (backgroundBrightness < 51) {
                f0 = 30;
                f1 = 50;
                if (elementBrightness == 0) {
                    rb = gb = bb = 96;
                }
            } else if (backgroundBrightness > 204) {
                f0 = 45;
                f1 = 70;
                if(elementBrightness == 254) {
                    rb = gb = bb = 192;
                }
            }else {
                f0 = 30 + (backgroundBrightness / 17);
                f1 = 50 + (backgroundBrightness * 20 / 255);
            }

            r = rb - (f0 * rb / 100);
            g = gb - (f0 * gb / 100);
            b = bb - (f0 * bb / 100);
            aResult[0] = "rgb(" + parseInt(r) + "," + parseInt(g) + "," + parseInt(b) + ")";

            r = rb + (f1 * (255 - rb) / 100);
            g = gb + (f1 * (255 - gb) / 100);
            b = bb + (f1 * (255 - bb) / 100);
            aResult[1] = "rgb(" + parseInt(r) + "," + parseInt(g) + "," + parseInt(b) + ")";

            return aResult;
        },

        makeBorderColor: function (aColor, aBackgroundColor, aBorderColorStyle) {
            var colors;
            var k = 0;
            switch (aBorderColorStyle) {
                case "none":
                    return "rgba(0, 0, 0, 0)";
                case "light":
                    k = 1;
                case "dark":
                    colors = this.getSpecial3DColors(aBackgroundColor, aColor);
                    return colors[k];
                case "solid":
                default:
                    return aColor;
            }
        },

        computeColorForLine: function (aLineIndex, aBorderColorStyle, aBorderColorStyleCount, aBorderColor, aBackgroundColor) {
            return this.makeBorderColor(aBorderColor, aBackgroundColor, aBorderColorStyle[aLineIndex]);
        },

        drawBorderSides: function (ctx, aSides) {
            if (aSides == 0)
                return;
            var borderRenderStyle;
            var borderRenderColor;
            var borderColorStyleTopLeft = [];
            var borderColorStyleBottomRight = [];
            var borderColorStyleCount = 0;
            var borderColorStyle = [];

            for (var i = 0; i < 4; i++) {
                if (((1 << i) & aSides) == 0)
                    continue;
                borderRenderStyle = this.borderStyles[i];
                borderRenderColor = this.borderColors[i];
                break;
            }

            if (borderRenderStyle == "none" ||
                borderRenderStyle == "hidden")
                return;


            if (this.oneUnitBorder && (borderRenderStyle == "ridge" ||
                        borderRenderStyle == "groove" ||
                        borderRenderStyle == "double"))
                borderRenderStyle = "solid";
            switch (borderRenderStyle) {
                case "solid":
                case "dashed":
                case "dotted":
                    borderColorStyleTopLeft[0] = "solid";
                    borderColorStyleBottomRight[0] = "solid";
                    borderColorStyleCount = 1;
                    break;
                case "groove":
                    borderColorStyleTopLeft[0] = "dark";
                    borderColorStyleTopLeft[1] = "light";
                    borderColorStyleBottomRight[0] = "light";
                    borderColorStyleBottomRight[1] = "dark";
                    borderColorStyleCount = 2;
                    break;
                case "ridge":
                    borderColorStyleTopLeft[0] = "light";
                    borderColorStyleTopLeft[1] = "dark";
                    borderColorStyleBottomRight[0] = "dark";
                    borderColorStyleBottomRight[1] = "light";
                    borderColorStyleCount = 2;
                    break;
                case "double":
                    borderColorStyleTopLeft[0] = "solid";
                    borderColorStyleTopLeft[1] = "none";
                    borderColorStyleTopLeft[2] = "solid";
                    borderColorStyleBottomRight[0] = "solid";
                    borderColorStyleBottomRight[1] = "none";
                    borderColorStyleBottomRight[2] = "solid";
                    borderColorStyleCount = 3;
                    break;
                case "inset":
                    borderColorStyleTopLeft[0] = "dark";
                    borderColorStyleBottomRight[0] = "light";
                    borderColorStyleCount = 1;
                    break;
                case "outset":
                    borderColorStyleTopLeft[0] = "light";
                    borderColorStyleBottomRight[0] = "dark";
                    borderColorStyleCount = 1;
                    break;
                default:
                    throw "Unhandled style '" + borderRenderStyle +"'";
                    break;
            }

            if (aSides & (this.BIT_RIGHT | this.BIT_BOTTOM))
                borderColorStyle = borderColorStyleBottomRight;
            else
                borderColorStyle = borderColorStyleTopLeft;

            var borderWidths= [[], [], []];
            if (borderColorStyleCount == 1) {
                for (var i = 0; i < 4; i++) {
                    borderWidths[0][i] = this.borderWidths[i];
                }
            } else if (borderColorStyleCount == 2) {
                for (var i = 0; i < 4; i++) {
                    var w = this.borderWidths[i];
                    borderWidths[0][i] = Math.floor(w / 2) + w % 2;
                    borderWidths[1][i] = Math.floor(w / 2);
                }
            } else if (borderColorStyleCount == 3) {
                for (var i = 0; i < 4; i++) {
                    var w = this.borderWidths[i];
                    if (w == 1) {
                        borderWidths[0][i] = 1.0;
                        borderWidths[1][i] = borderWidths[2][i] = 0.0;
                    } else {
                        var rest = w % 3;
                        borderWidths[0][i] = borderWidths[2][i] = borderWidths[1][i] = Math.floor((w - rest) / 3);
                        if (rest == 1) {
                            borderWidths[1][i]++;
                        } else if (rest == 2) {
                            borderWidths[0][i]++;
                            borderWidths[2][i]++;
                        }
                    }
                }
            }

            var oRect = new th.Rectangle(this.outerRect);
            var iRect = new th.Rectangle(this.outerRect);

            var radii = this.copyRadii(this.borderRadii);

            for (var i = 0; i < borderColorStyleCount; i++) {
                iRect.inset(borderWidths[i]);

                if (borderColorStyle[i] != "none") {
                    var color = this.computeColorForLine(i, borderColorStyle,
                            borderColorStyleCount, borderRenderColor,
                            this.backgroundColor);
                    this.fillSolidBorder(ctx, oRect, iRect, radii, borderWidths[i], aSides, color);
                }

                radii = this.computeInnerRadii(radii, borderWidths[i]);

                oRect.x = iRect.x;
                oRect.y = iRect.y;
                oRect.w = iRect.w;
                oRect.h = iRect.h;
            }
        },

        paint: function (ctx) {
            var b = this.component.d();

            this.recalculateBorderData();

            var tlBordersSame = this.areBorderSideFinalStylesSame(1|8); // top+left
            var brBordersSame = this.areBorderSideFinalStylesSame(2|4); // bottom+right
            var allBordersSame = this.areBorderSideFinalStylesSame(15); // all sides

            if (allBordersSame && (this.borderStyles[0] == "none" ||
                        this.borderStyles[0] == "hidden"))
                return;

            this.outerRect.round();
            this.innerRect.round();

            if (this.outerRect.isEmpty())
                return;

            if (allBordersSame) {
                this.drawBorderSides(ctx, this.BIT_TOP | this.BIT_RIGHT | this.BIT_BOTTOM | this.BIT_LEFT);
            } else {
                for (var corner = 0; corner < 4; corner++) {
                    var sides = [corner, this.prev(corner)];
                    if (!this.borderRadii[corner].isZero())
                        continue;

                    if (this.borderWidths[sides[0]] == 1 && this.borderWidths[sides[1]] == 1) {
                        if (corner == this.TOP_LEFT || corner == this.TOP_RIGHT)
                            this.cornerDimensions[corner].w = 0;
                        else
                            this.cornerDimensions[corner].h = 0;
                    }
                }

                for (var corner = 0; corner < 4; corner++) {
                    if (this.cornerDimensions[corner].isZero())
                        continue;

                    var sides = [corner, this.prev(corner)];
                    var sideBits = (1 << sides[0]) | (1 << sides[1]);

                    var simpleCornerStyle = this.areBorderSideFinalStylesSame(sideBits);
                    if (simpleCornerStyle && this.borderRadii[corner].isZero() && this.isSolidCornerStyle(this.borderStyles[sides[0]], corner)) {
                        ctx.beginPath();
                        this.doCornerSubPath(ctx, corner);
                        ctx.fillStyle = this.makeBorderColor(
                            this.borderColors[sides[0]], this.backgroundColor,
                            this.borderColorStyleForSolidCorner(this.borderStyles[sides[0]], corner)
                        );
                        ctx.fill();
                        continue;
                    }

                    ctx.save();
                    ctx.beginPath();
                    this.doCornerSubPath(ctx, corner);
                    ctx.clip();

                    if (simpleCornerStyle) {
                        this.drawBorderSides(ctx, sideBits);
                    } else {
                        var tmpctx = this.component.getScratchContext();
                        tmpctx.canvas.height = b.b.h;
                        tmpctx.canvas.width = b.b.w;
                        tmpctx.clearRect(0, 0, b.b.w, b.b.h);
                        tmpctx.globalCompositeOperation = "lighter"; // closest thing i have to "add"
                        for (var cornerSide = 0; cornerSide < 2; cornerSide++) {
                            var side = sides[cornerSide];
                            var style = this.borderStyles[side];
                            tmpctx.save();
                            tmpctx.beginPath();
                            this.doSideClipSubPath(tmpctx, side);
                            tmpctx.clip();
                            this.drawBorderSides(tmpctx, 1 << side);
                            tmpctx.restore();
                        }
                        ctx.globalCompositeOpertation = "source-over";
                        ctx.drawImage(tmpctx.canvas, 0, 0, b.b.w, b.b.h, 0, 0, b.b.w, b.b.h);
                    }
                    ctx.restore();
                }
                var alreadyDrawnSides = 0;
                if (this.oneUnitBorder && this.noBorderRadius) {
                    if (tlBordersSame) {
                        this.drawBorderSides(ctx, 1 | 8); // top + left
                        alreadyDrawnSides |= 1 | 8;
                    }
                    if (brBordersSame) {
                        this.drawBorderSides(ctx, 4 | 2);
                        alreadyDrawnSides |= 4 | 2;
                    }
                }
                for (var side = 0; side < 4; side++) {
                    if (alreadyDrawnSides & (1 << side))
                        continue;

                    if (this.borderWidths[side] == 0 ||
                        this.borderStyles[side] == "hidden" ||
                        this.borderStyles[side] == "none")
                        continue;

                    ctx.save();
                    ctx.beginPath();
                    this.doSideClipWithoutCornersSubPath(ctx, side);
                    ctx.clip();
                    this.drawBorderSides(ctx, 1 << side);
                    ctx.restore();
                }
            }
        },

        computeInnerRadii: function (aRadii, borderSizes) {
            var radii = {};
            radii[this.TOP_LEFT] = new th.Size2D(
                Math.max(0.0, aRadii[this.TOP_LEFT].w - borderSizes[3]),
                Math.max(0.0, aRadii[this.TOP_LEFT].h - borderSizes[0])
            );
            radii[this.TOP_RIGHT] = new th.Size2D(
                Math.max(0.0, aRadii[this.TOP_RIGHT].w - borderSizes[1]),
                Math.max(0.0, aRadii[this.TOP_RIGHT].h - borderSizes[0])
            );
            radii[this.BOTTOM_RIGHT] = new th.Size2D(
                Math.max(0.0, aRadii[this.BOTTOM_RIGHT].w - borderSizes[1]),
                Math.max(0.0, aRadii[this.BOTTOM_RIGHT].h - borderSizes[2])
            );
            radii[this.BOTTOM_LEFT] = new th.Size2D(
                Math.max(0.0, aRadii[this.BOTTOM_LEFT].w - borderSizes[3]),
                Math.max(0.0, aRadii[this.BOTTOM_LEFT].h - borderSizes[2])
            );
            return radii;
        },

        copyRadii: function (radii) {
            var r = {};
            for (c in radii) {
                r[c] = new th.Size2D(radii[c]);
            }
            return r;
        },

        traceRoundRect: function (ctx, rect, radii, clockwise) {
            if (clockwise) {
                ctx.moveTo(rect.x, rect.y + radii[this.TOP_LEFT].h);
                ctx.quadraticCurveTo(rect.x, rect.y, rect.x + radii[this.TOP_LEFT].w, rect.y);
                ctx.lineTo(rect.x + rect.w - radii[this.TOP_RIGHT].w, rect.y);
                ctx.quadraticCurveTo(rect.x + rect.w, rect.y, rect.x + rect.w, rect.y + radii[this.TOP_RIGHT].h);
                ctx.lineTo(rect.x + rect.w, rect.y + rect.h - radii[this.BOTTOM_RIGHT].h);
                ctx.quadraticCurveTo(rect.x + rect.w, rect.y + rect.h, rect.x + rect.w - radii[this.BOTTOM_RIGHT].w, rect.y + rect.h);
                ctx.lineTo(rect.x + radii[this.BOTTOM_LEFT].w, rect.y + rect.h);
                ctx.quadraticCurveTo(rect.x, rect.y + rect.h, rect.x, rect.y + rect.h - radii[this.BOTTOM_LEFT].h);
                ctx.lineTo(rect.x, rect.y + radii[this.TOP_LEFT].h);
            } else {
                ctx.moveTo(rect.x, rect.y + radii[this.TOP_LEFT].h);
                ctx.lineTo(rect.x, rect.y + rect.h - radii[this.BOTTOM_LEFT].h)
                ctx.quadraticCurveTo(rect.x, rect.y + rect.h, rect.x + radii[this.BOTTOM_LEFT].w, rect.y + rect.h);
                ctx.lineTo(rect.x + rect.w - radii[this.BOTTOM_RIGHT].w, rect.y + rect.h);
                ctx.quadraticCurveTo(rect.x + rect.w, rect.y + rect.h, rect.x + rect.w, rect.y + rect.h - radii[this.BOTTOM_RIGHT].h);
                ctx.lineTo(rect.x + rect.w, rect.y + radii[this.TOP_RIGHT].h);
                ctx.quadraticCurveTo(rect.x + rect.w, rect.y, rect.x + rect.w - radii[this.TOP_RIGHT].w, rect.y);
                ctx.lineTo(rect.x + radii[this.TOP_LEFT].w, rect.y);
                ctx.quadraticCurveTo(rect.x, rect.y, rect.x, rect.y + radii[this.TOP_LEFT].h);
            }
        }
    }
});


th.Component = Class.define({
    type: "Component",

    uses: [
        th.ComponentHelpers
    ],

    members: {
        init: function(parms) {
            if (!parms) parms = {};
            for (parm in parms) {
                this[parm] = parms[parm];
            }

            if (!this.bounds) this.bounds = {};

            this.border = new th.SimpleBorder(this);

            if (this.opaque == undefined) this.opaque = true;

            this.bus = th.global_event_bus;

            this.styles = undefined;        // this is explicitly undefined so I can catch situations where we add to this directly

            this.localStyles = {};

            this.refreshCss = true;
        },

        getScratchContext: function() {
            var scene = this.getScene();
            if (scene) return scene.scratchContext;
        },

        getInsets: function() {
            var insets = this.border.getInsets();

            this.calculateInsets("padding-", "", insets);

            return insets;
        },

        getMargins: function() {
            return this.calculateInsets("margin-");
        },

        paint: function(ctx) {},

        repaint: function() {
            if (!this.getScene()) return;

            this.getScene().paint(this);
        }
    }
});

th.SimpleLayout = Class.define({
    members: {
        init: function(parms) {
            if (!parms) parms = {};
            this.orientation = parms.orientation || th.HORIZONTAL;
        },

        layout: function(container) {
            var d = container.d();
            if (container.children.length > 0) {
                var h = (this.orientation == th.HORIZONTAL);

                var layoutLength = (h) ? d.b.iw : d.b.ih;

                var componentLength = parseInt(layoutLength / container.children.length);

                var remainder = layoutLength % container.children.length;

                var currentPosition = (h) ? d.i.l : d.i.t;  // the dimension that is variable (e.g., x axis when laying comp
                var constantAxisPosition = currentPosition; // the dimension that is constant (e.g., y axis when laying components out horizontally)
                for (var i = 0; i < container.children.length; i++) {
                    var r = 0;
                    if (remainder > 0) {
                        remainder--;
                        r = 1;
                    }

                    var variableAxisPosition = currentPosition;
                    var length = componentLength + r;

                    container.children[i].bounds = (h) ? {
                        x:      variableAxisPosition,
                        y:      constantAxisPosition,
                        width:  length,
                        height: d.b.ih
                    } : {
                        x:      constantAxisPosition,
                        y:      variableAxisPosition,
                        width:  d.b.ih,
                        height: d.b.iw
                    }

                    currentPosition += length;
                }
            }

        },

        getMinimumSize: function(container, type) {
            var dimension = (this.orientation == th.HORIZONTAL) ? "width" : "height";
            var size = 0;
            for (var i = 0; i < container.children.length; i++) {
                size += container.children[i].getMinimumSize()[dimension];
            }
            return size;
        },

        getPreferredSize: function(container) {
            var dimension = (this.orientation == th.HORIZONTAL) ? "width" : "height";
            var size = 0;
            for (var i = 0; i < container.children.length; i++) {
                size += container.children[i].getPreferredSize()[dimension];
            }
            return size;
        },

        getMaximumSize: function(container) {
            var dimension = (this.orientation == th.HORIZONTAL) ? "width" : "height";
            var size = 0;
            for (var i = 0; i < container.children.length; i++) {
                size += container.children[i].getMaximumSize()[dimension];
            }
            return size;
        }
    }
});

th.Container = Class.define({
    type: "Container",

    superclass: th.Component,

    uses: [
        th.ContainerHelpers
    ],

    members: {
        init: function(parms) {
            this._super(parms);
            this.children = [];

            this.checkedForFormLayout = false;

            this.layoutManager = new th.SimpleLayout();
        },

        add: function() {
            for (var z = 0; z < arguments.length; z++) {
                component = th.isArray(arguments[z]) ? arguments[z] : [ arguments[z] ];
                this.children = this.children.concat(component);
                for (var i = 0; i < component.length; i++) {
                    component[i].parent = this;
                    component[i].refreshCss = true;
                }
            }
        },

        remove: function() {
            for (var z = 0; z < arguments.length; z++) {
                component = th.isArray(arguments[z]) ? arguments[z] : [ arguments[z] ];
                for (var i = 0; i < component.length; i++) {
                    var old_length = this.children.length;
                    this.children = th.remove(this.children, component[i]);

                    if (old_length != this.children.length) delete component[i].parent;
                }
            }
        },

        replace: function(component, index) {
            this.bus.unbind(this.children[index]);
            component.parent = this;
            this.children[index] = component;
        },

        paint: function(ctx) {
            if (this.shouldPaint()) {
                this.paintSelf(ctx);
                this.paintChildren(ctx);
            }
        },

        paintSelf: function(ctx) {},

        paintChildren: function(ctx) {
            for (var i = 0; i < this.children.length; i++ ) {
                if (!this.children[i].shouldPaint()) continue;

                if (!this.children[i].bounds) {
                    continue;
                }

                ctx.save();
                try {
                    ctx.translate(this.children[i].bounds.x, this.children[i].bounds.y);
                } catch (error) {
                    ctx.restore();
                    continue;
                }

                try {
                        ctx.beginPath();
                        ctx.rect(0, 0, this.children[i].bounds.width, this.children[i].bounds.height);
                        ctx.closePath();
                        ctx.clip();
                } catch(ex) {
                }

                ctx.save();
                this.children[i].paint(ctx);
                ctx.restore();

                if (this.children[i].border) {
                    ctx.save();
                    this.children[i].border.paint(ctx);
                    ctx.restore();
                }

                ctx.restore();
            }
        },

        installLayoutManager: function(layout) {
            this.layoutManager = layout;

            var constraints = false;
            for (var i = 0; i < this.children.length; i++) {
                if (this.children[i]["constraints"] !== undefined) {
                    if (!this.children[i].cssValue("-th-constraints")) this.children[i].addCss("-th-constraints", this.children[i]["constraints"]);
                }

                if (this.children[i].cssValue("-th-constraints")) {
                    constraints = true;
                }
            }

            if (layout instanceof th.formlayout.FormLayout) {
                if (!constraints) {
                    var row = 1;
                    var col = 1;
                    th.forEach(this.children, function(component) {
                        var found = false;
                        while (row <= layout.getRowCount()) {
                            while (col <= layout.getColumnCount()) {
                                var colspec = layout.getColumnSpec(col);
                                if (colspec.isSpacer()) {
                                    col++;
                                    continue;
                                }
                                found = true;
                                break;
                            }
                            if (found) {
                                var rowspec = layout.getRowSpec(row);
                                if (!rowspec.isSpacer()) break;
                            }
                            found = false;
                            row++;
                            col = 1;
                        }
                        if (!found) return;
                        layout.addLayoutComponent(component, new th.formlayout.CellConstraints(col, row));
                        col++;
                    });
                } else {
                    th.forEach(this.children, function(component) {
                        var constraints = component.cssValue("-th-constraints");
                        layout.addLayoutComponent(component, new th.formlayout.CellConstraints(constraints));
                    });
                }
            }
        },

        layoutTree: function() {
            if (!this.checkedForFormLayout) {
                this.checkedForFormLayout = true;

                if (this.grid) {
                    var cr = this.grid.split(";");
                    if (cr.length != 2) {
                        console.log("Couldn't parse 'grid' property: " + this.grid);
                    } else {
                        this.addCss("-th-grid-cols", cr[0]);
                        this.addCss("-th-grid-rows", cr[1]);
                    }
                }

                var cols = this.cssValue("-th-grid-cols");
                var rows = this.cssValue("-th-grid-rows");

                if (cols && rows) this.installLayoutManager(new th.formlayout.FormLayout(cols, rows));
            }

            this.layout();

            for (var i = 0; i < this.children.length; i++) {
                if (this.children[i].layoutTree) this.children[i].layoutTree();
            }
        },

        layout: function() {
            if (this.layoutManager) this.layoutManager.layout(this);
        },

        render: function() {
            if (!th.global_resources.loaded) return;

            this.layoutTree();
            this.repaint();
        }
    }
});

th.Window = Class.define({
    type: "Window",

    members: {
        init: function(parms) {
            parms = parms || {};

            this.containerId = parms.containerId || false;
            this.width = parms.width || 200;
            this.height = parms.height || 300;
            this.title = parms.title || 'NO TITLE GIVEN!';
            this.y = parms.top || 50;
            this.x = parms.left || 50;
            this.isVisible = false;
            this.closeOnClickOutside = !!parms.closeOnClickOutside;

            if(!parms.containerId) {
                console.error('The "containerId" must be given!');
                return;
            }

            if (th.byId(this.containerId)) {
                console.error('There is already a element with the id "'+this.containerId+'"!');
                return;
            }

            if (!parms.userPanel) {
                console.error('The "userPanel" must be given!');
                return;
            }

            /*if (!th.byId('popup_insert_point')) {
                for (var x = 0; x < document.childNodes.length; x++) {
                    if (document.childNodes[x].nodeType == 1) {
                        var popupParent = document.createElement("div");
                        popupParent.id = 'popup_insert_point';
                        document.childNodes[x].appendChild(popupParent);
                        break;
                    }
                }
            }*/

            th.byId('popup_insert_point').innerHTML += '<div id="'+this.containerId+'" class="popupWindow"></div>';
            this.container = th.byId(this.containerId);
            dojo.attr(this.container, { width: this.width, height: this.height, tabindex: '-1' });

            this.container.innerHTML = "<canvas id='"+this.containerId+"_canvas'></canvas>";
            this.canvas = th.byId(this.containerId + '_canvas');
            dojo.attr(this.canvas, { width: this.width, height: this.height, tabindex: '-1' });

            this.scene = new th.Scene(this.canvas);
            this.windowPanel = new th.components.WindowPanel(parms.title, parms.userPanel);
            this.windowPanel.windowBar.parentWindow = this;
            this.scene.root.add(this.windowPanel);

            this.move(this.x, this.y);


            dojo.connect(window, "mousedown", dojo.hitch(this, function(e) {
                if (!this.isVisible || !this.closeOnClickOutside) return;

                var d = dojo.coords(this.container);
                if (e.clientX < d.l || e.clientX > (d.l + d.w) || e.clientY < d.t || e.clientY > (d.t + d.h)) {
                    this.toggle();
                } else {
                    dojo.stopEvent(e);
                }
            }));

            dojo.connect(window, "keydown", dojo.hitch(this, function(e) {
                if (!this.isVisible) return;

                if(e.keyCode == bespin.util.keys.Key.ESCAPE) {
                    this.toggle();
                    dojo.stopEvent(e);
                }
            }));
        },

        toggle: function() {
            this.isVisible = !this.isVisible;

            if (this.isVisible) {
                this.container.style.display = 'block';
                this.layoutAndRender();
            } else {
                this.container.style.display = 'none';
            }

            this.scene.bus.fire("toggle", {isVisible: this.isVisible}, this);
        },

        layoutAndRender: function() {
            this.scene.layout();
            this.scene.render();
        },

        centerUp: function() {
            this.move(Math.round((window.innerWidth - this.width) * 0.5), Math.round((window.innerHeight - this.height) * 0.25));
        },

        center: function() {
            this.move(Math.round((window.innerWidth - this.width) * 0.5), Math.round((window.innerHeight - this.height) * 0.5));
        },

        move: function(x, y) {
            this.y = y;
            this.x = x;
            this.container.style.top = y + 'px';
            this.container.style.left = x + 'px';
        },

        getPosition: function() {
            return { x: this.x, y: this.y };
        }
    }
});

if (typeof th == "undefined") th = {};

th.Panel = Class.define({
    type: "Panel",

    superclass: th.Container,

    members: {
        init: function(parms) {
            this._super(parms);
        },

        paintSelf: function(ctx) {
            this.paintBackground(ctx);
        }
    }
});

th.Button = Class.define({
    type: "Button",

    superclass: th.Component,

    members: {
        init: function(parms) {
            this._super(parms);
        },

        paint: function(ctx) {
            var d = this.d();

            var top = this.cssValue("-th-top-image");
            var mid = this.cssValue("-th-middle-image");
            var bot = this.cssValue("-th-bottom-image");

            if (top) top = th.global_resources.images[top];
            if (mid) mid = th.global_resources.images[mid];
            if (bot) bot = th.global_resources.images[bot];

            if (top && mid && bot) {
                if (d.b.h >= top.height + bot.height) {
                    ctx.drawImage(top, 0, 0);
                    if (d.b.h > top.height + bot.height) {
                        ctx.drawImage(mid, 0, top.height, mid.width, d.b.h - top.height - bot.height);
                    }
                    ctx.drawImage(bot, 0, d.b.h - bot.height);
                }
            } else {
                this.paintBackground(ctx);
            }
        }
    }
});

th.Scrollbar = Class.define({
    type: "Scrollbar",

    superclass: th.Container,

    members: {
        init: function(parms) {
            if (!parms) parms = {};

            this._super(parms);

            this.orientation = parms.orientation || th.VERTICAL;
            this.value = parms.value || 0;
            this.min = parms.min || 0;
            this.max = parms.max || 100;
            this.extent = parms.extent || 0.1;
            this.increment = parms.increment || 2;

            this.up = new th.Button({ className: "up" });
            this.down = new th.Button({ className: "down" });
            this.bar = new th.Button({ className: "bar" });
            this.add([ this.up, this.down, this.bar ]);

            this.bus.bind("click", this.up, this.scrollup, this);
            this.bus.bind("click", this.down, this.scrolldown, this);
            this.bus.bind("mousedrag", this.bar, this.onmousedrag, this);
            this.bus.bind("mouseup", this.bar, this.onmouseup, this);
        },

        onmousedrag: function(e) {
            var currentPosition = (this.orientation == th.VERTICAL) ? e.clientY : e.clientX;

            if (this.dragstart_value == undefined) {
                this.dragstart_value = this.value;
                this.dragstart_mouse = currentPosition;
                return;
            }

            var diff = currentPosition - this.dragstart_mouse;  // difference in pixels; needs to be translated to a difference in value

            var pixel_range = this.bounds.height - this.up.bounds.height - this.down.bounds.height - this.bar.bounds.height; // total number of pixels that map to the value range

            var pixel_to_value_ratio = (this.max - this.min) / pixel_range;

            this.value = this.dragstart_value + Math.floor(diff * pixel_to_value_ratio);
            if (this.value < this.min) this.value = this.min;
            if (this.value > this.max) this.value = this.max;
            if (this.scrollable) this.scrollable.scrollTop = this.value;
            this.render();
            if (this.scrollable) this.scrollable.repaint();
        },

        onmouseup: function(e) {
            delete this.dragstart_value;
            delete this.dragstart_mouse;
        },

        scrollup: function(e) {
            if (this.value > this.min) {
                this.value = Math.max(this.min, this.value - this.increment);
                if (this.scrollable) this.scrollable.scrollTop = this.value;
                this.render();
                if (this.scrollable) this.scrollable.repaint();
            }
        },

        scrolldown: function(e) {
            if (this.value < this.max) {
                this.value = Math.min(this.max, this.value + this.increment);
                if (this.scrollable) this.scrollable.scrollTop = this.value;
                this.render();
                if (this.scrollable) this.scrollable.repaint();
            }
        },

        layout: function() {
            var d = this.d();

            if (this.scrollable) {
                var view_height = this.scrollable.bounds.height;
                var scrollable_info = this.scrollable.getScrollInfo();
                this.min = 0;
                this.max = scrollable_info.scrollHeight - view_height;
                this.value = scrollable_info.scrollTop;
                this.extent = (scrollable_info.scrollHeight - view_height) / scrollable_info.scrollHeight;
            }

            if (this.max < this.min) {
                for (var i = 0; i < this.children.length; i++) delete this.children[i].bounds;
                return;
            }

            if (this.orientation == th.VERTICAL) {
                var w = d.b.iw;
                var h = 15;
                this.up.bounds = { x: d.i.l + 1, y: d.i.t, width: w, height: h };
                this.down.bounds = { x: d.i.l + 1, y: d.b.ih - h, width: w, height: h };

                var scroll_track_height = d.b.ih - this.up.bounds.height - this.down.bounds.height;

                var extent_length = Math.min(Math.floor(scroll_track_height - (this.extent * scroll_track_height), d.b.ih - this.up.bounds.height - this.down.bounds.height));
                var extent_top = Math.floor(this.up.bounds.height + Math.min( (this.value / (this.max - this.min)) * (scroll_track_height - extent_length) ));
                this.bar.bounds = { x: d.i.l + 1, y: extent_top, width: d.b.iw, height: extent_length };
            } else {

            }
        },

        paint: function(ctx) {
            if (this.max < 0) return;

            var top = this.cssValue("-th-vertical-top-image");
            var mid = this.cssValue("-th-vertical-middle-image");
            var bot = this.cssValue("-th-vertical-bottom-image");

            if (top) top = th.global_resources.images[top];
            if (mid) mid = th.global_resources.images[mid];
            if (bot) bot = th.global_resources.images[bot];

            if (top) ctx.drawImage(top, 1, this.up.bounds.height);
            if (mid) ctx.drawImage(mid, 1, this.up.bounds.height + top.height, mid.width, this.down.bounds.y - this.down.bounds.height - (this.up.bounds.x - this.up.bounds.height));
            if (bot) ctx.drawImage(bot, 1, this.down.bounds.y - bot.height);

            this._super(ctx);
        }
    }
});

th.ResizeNib = Class.define({
    type: "ResizeNib",

    superclass: th.Component,

    members: {
        init: function(parms) {
            this._super(parms);

            this.bus.bind("mousedown", this, this.onmousedown, this);
            this.bus.bind("mouseup", this, this.onmouseup, this);
            this.bus.bind("mousedrag", this, this.onmousedrag, this);
        },

        onmousedown: function(e) {
            this.startPos = { x: e.clientX, y: e.clientY};
        },

        onmousedrag: function(e) {
            if (this.startPos) {
                if (!this.firedDragStart) {
                    this.bus.fire("dragstart", this.startPos, this);
                    this.firedDragStart = true;
                }

                this.bus.fire("drag", { startPos: this.startPos, currentPos: { x: e.clientX, y: e.clientY } }, this);
            }
        },

        onmouseup: function(e) {
            if (this.startPos && this.firedDragStart) {
                this.bus.fire("dragstop", { startPos: this.startPos, currentPos: { x: e.clientX, y: e.clientY } }, this);
                delete this.firedDragStart;
            }
            delete this.startPos;
        },

        paint: function(ctx) {
            var d = this.d();

            if (this.orientation == th.VERTICAL) {
                var bw = 7;
                var x = Math.floor((d.b.w / 2) - (bw / 2));
                var y = 7;

                ctx.fillStyle = this.cssValue("-th-vertical-bar-shadow-color");
                for (var i = 0; i < 3; i++) {
                    ctx.fillRect(x, y, bw, 1);
                    y += 3;
                }

                y = 8;
                ctx.fillStyle = this.cssValue("-th-vertical-bar-color");
                for (var i = 0; i < 3; i++) {
                    ctx.fillRect(x, y, bw, 1);
                    y += 3;
                }
            } else {
                var bh = 7;

                var dw = 8; // width of the bar area
                var dh = bh + 2; // height of the bar area

                var x = Math.floor(d.b.w / 2 - (dw / 2));
                var y = Math.floor(d.b.h / 2 - (dh / 2));

                var cx = x;

                if (this.cssValue("-th-horizontal-bar-subtle-shadow-color")) {
                    ctx.fillStyle = this.cssValue("-th-horizontal-bar-subtle-shadow-color");
                    for (var i = 0; i < 3; i++) {
                        ctx.fillRect(cx, y, 1, dh);
                        cx += 3;
                    }
                }

                cx = x + 1;
                ctx.fillStyle = this.cssValue("-th-horizontal-bar-shadow-color");
                for (var i = 0; i < 3; i++) {
                    ctx.fillRect(cx, y + dh - 1, 1, 1);
                    cx += 3;
                }

                cx = x + 1;
                ctx.fillStyle = this.cssValue("-th-horizontal-bar-color");
                for (var i = 0; i < 3; i++) {
                    ctx.fillRect(cx, y + 1, 1, bh);
                    cx += 3;
                }
            }
        }
    }
});

/*
    A "splitter" that visually demarcates areas of an interface. Can also have some "nibs" on its ends to facilitate resizing.
    Provides "dragstart", "drag", and "dragstop" events that are fired when a nib is dragged. Orientation is in terms of a container and
    is confusing; HORIZONTAL means the splitter is actually displayed taller than wide--what might be called vertically, and similarly
    VERTICAL means the splitter is wider than it is tall, i.e., horizontally. This is because the *container* is laid out such that
    different regions are stacked horizontally or vertically, and the splitter demarcates those areas.

    This bit of confusion was deemed better than having the orientation for a hierarchy of components be different but contributing to the
    same end.

    Note also that this component uses getPreferredHeight() and getPreferredWidth() differently than most; only one of the methods is
    valid for a particular orientation. I.e., when in HORIZONTAL orientation, getPreferredWidth() should be used and getPreferredHeight()
    ignored.

 */
th.Splitter = Class.define({
    type: "Splitter",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            this._super(parms);

            this.topNib = new th.ResizeNib({ orientation: this.orientation } );
            this.bottomNib = new th.ResizeNib({ orientation: this.orientation } );
            this.add(this.topNib, this.bottomNib);

            this.label = parms.label;
            if (this.label) this.add(this.label);

            this.scrollbar = parms.scrollbar;
            if (this.scrollbar) this.add(this.scrollbar);

            this.bus.bind("drag", [ this.topNib, this.bottomNib ], this.ondrag, this);
            this.bus.bind("dragstart", [ this.topNib, this.bottomNib ], this.ondragstart, this);
            this.bus.bind("dragstop", [ this.topNib, this.bottomNib ], this.ondragstop, this);
        },

        ondrag: function(e) {
            this.bus.fire("drag", e, this);
        },

        ondragstart: function(e) {
            this.bus.fire("dragstart", e, this);
        },

        ondragstop: function(e) {
            this.bus.fire("dragstop", e, this);
        },

        getPreferredSize: function() {
            return { height: 20, width: 16 };
        },

        layout: function() {
            var d = this.d();

            if (!this.orientation) this.orientation = (this.bounds.height > this.bounds.width) ? th.HORIZONTAL : th.VERTICAL;

            if (this.orientation == th.HORIZONTAL) {
                this.topNib.bounds = { x: 0, y: 0, height: d.b.w, width: d.b.w };
                this.bottomNib.bounds = { x: 0, y: this.bounds.height - d.b.w, height: d.b.w, width: d.b.w };

                if (this.scrollbar && this.scrollbar.shouldLayout()) {
                    this.scrollbar.bounds = { x: 0, y: this.topNib.bounds.height, height: d.b.h - (this.topNib.bounds.height * 2), width: d.b.w };
                }
            } else {
                this.topNib.bounds = { x: 0, y: 0, height: d.b.h, width: d.b.h };
                this.bottomNib.bounds = { x: d.b.w - d.b.h, y: 0, height: d.b.h, width: d.b.h };

                if (this.label) {
                    this.label.bounds = { x: this.topNib.bounds.x + this.topNib.bounds.width, y: 0, height: d.b.h, width: d.b.w - (d.b.h * 2) };
                }
            }
        },

        paintSelf: function(ctx) {
            this.className = (this.orientation == th.VERTICAL) ? "vertical" : "horizontal";
            this.resolveCss();
            this._super(ctx);
        }
    }
});

th.SplitPanelContainer = Class.define({
    type: "SplitPanelContainer",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            this._super(parms);

            this.splitter = new th.Splitter({ orientation: this.orientation, label: parms.label });
        },

        getContents: function() {
            var childrenWithoutSplitter = th.remove(this.children, this.splitter);
            if (childrenWithoutSplitter.length > 0) return childrenWithoutSplitter[0];
        },

        layout: function() {
            var childrenWithoutSplitter = th.remove(this.children, this.splitter);
            if (this.children.length == childrenWithoutSplitter.length) this.add(this.splitter);

            var prefSize = this.splitter.getPreferredSize();
            var slength = (this.orientation == th.HORIZONTAL) ?
                          prefSize.width :
                          prefSize.height;
            if (this.splitter.shouldLayout()) {
                if (this.orientation == th.HORIZONTAL) {
                    this.splitter.bounds = { x: this.bounds.width - slength, y: 0, height: this.bounds.height, width: slength };
                } else {
                    this.splitter.bounds = { x: 0, y: this.bounds.height - slength, height: slength, width: this.bounds.width };
                }
            } else {
                slength = 0;
            }

            if (childrenWithoutSplitter.length > 0) {
                if (this.orientation == th.HORIZONTAL) {
                    childrenWithoutSplitter[0].bounds = { x: 0, y: 0, height: this.bounds.height, width: this.bounds.width - slength }
                } else {
                    childrenWithoutSplitter[0].bounds = { x: 0, y: 0, height: this.bounds.height - slength, width: this.bounds.width }
                }
            }
        }
    }
});

/*
    A component that allocates all visible space to two or more nested regions.
 */
th.SplitPanel = Class.define({
    type: "SplitPanel",

    superclass: th.Panel,

    uses: [
        th.StringHelpers
    ],

    members: {
        init: function(parms) {
            this._super(parms);

            if (!this.orientation) this.orientation = th.HORIZONTAL;

            if (!this.regions) this.regions = [{},{}];
        },

        ondragstart: function(e) {
            var container = e.thComponent.parent; // splitter -> splitpanecontainer
            container.region.startSize = container.region.size;
        },

        ondrag: function(e) {
            var container = e.thComponent.parent; // splitter -> splitpanecontainer

            var delta = (this.orientation == th.HORIZONTAL) ? e.currentPos.x - e.startPos.x : e.currentPos.y - e.startPos.y;

            container.region.size = container.region.startSize + delta;
            this.render();
        },

        ondragstop: function(e) {
            var container = e.thComponent.parent; // splitter -> splitpanecontainer
            delete container.region.startSize;
        },

        layout: function() {
            this.remove(this.children); // remove any of the existing region panels

            /*
               iterate through each region, performing a couple of tasks:
                - create a container for each region if it doesn't already have one
                - put the value of the contents property of region into the container if necessary
                - hide the splitter on the last region
             */
            for (var i = 0; i < this.regions.length; i++) {
                var region = this.regions[i];
                if (!region.container) {
                    region.container = new th.SplitPanelContainer({ orientation: this.orientation, label: region.label });

                    region.container.region = region;   // give the container a reference back to the region

                    this.bus.bind("dragstart", region.container.splitter, this.ondragstart, this);
                    this.bus.bind("drag", region.container.splitter, this.ondrag, this);
                    this.bus.bind("dragstop", region.container.splitter, this.ondragstop, this);
                }

                if (region.contents && (region.contents != region.container.getContents())) {
                    region.container.removeAll();
                    region.container.add(region.contents);
                }

                if (i == this.regions.length - 1) region.container.splitter.addCss("display", "none");

                this.add(region.container);
            }

            var containerSize = (this.orientation == th.HORIZONTAL) ? this.bounds.width : this.bounds.height;

            var totalSize = 0;
            for (var i = 0; i < this.regions.length; i++) {
                var r = this.regions[i];

                if (!r.size) {
                    r.size = (this.defaultSize || (100 / this.regions.length) + "%");
                }

                if (this.isPercentage(r.size)) {
                    r.size = Math.floor((parseInt(r.size) / 100) * containerSize);
                }

                if (r.size < 30) r.size = 30;

                totalSize += r.size;
            }
            if (totalSize > containerSize) {   // if the regions are bigger than the split pane size, shrink 'em, right-to-left
                var diff = totalSize - containerSize;
                for (var i = this.regions.length - 1; i >= 0; i--) {
                    var r = this.regions[i];

                    var originalSize = r.size;
                    r.size -= diff;
                    if (r.size < 30) r.size = 30;
                    diff -= (originalSize - r.size);

                    if (diff <= 0) break;
                }
            } else if (totalSize < containerSize) {    // if the regions are smaller, grow 'em, all in the last one
                var r = this.regions[this.regions.length - 1].size += (containerSize - totalSize);
            }

            var startPx = 0;
            for (var i = 0; i < this.regions.length; i++) {
                var region = this.regions[i];
                if (this.orientation == th.HORIZONTAL) {
                    region.container.bounds = { x: startPx, y: 0, width: region.size, height: this.bounds.height };
                } else {
                    region.container.bounds = { x: 0, y: startPx, width: this.bounds.width, height: region.size };
                }
                startPx += region.size;

            }
        }
    }
});

th.Label = Class.define({
    type: "Label",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            if (!parms) parms = {};

            this._super(parms);

            this.text = parms.text || "";
        },

        styleContext: function(ctx) {
            if (!ctx) return;

            ctx.font = this.cssValue("font");
            ctx.fillStyle = this.cssValue("color");

            return ctx;
        },

        getPreferredSize: function() {
            var ctx = this.styleContext(this.parent.getScratchContext());

            var w = ctx.measureText(this.text).width + 2;
            w += this.getInsets().left + this.getInsets().right;

            var h = Math.floor(ctx.measureText(this.text).ascent * 1.5);   // multiplying by 2 to fake a descent and leading
            h += this.getInsets().top + this.getInsets().bottom;

            return { height: h, width: w };
        },

        paint: function(ctx) {
            this._super(ctx);

            var d = this.d();

            this.styleContext(ctx);

            var textMetrics = ctx.measureText(this.text);

            var textToRender = this.text;
            if (!textToRender) textToRender = "";
            var lastLength = textToRender.length - 2;
            while (textMetrics.width > (d.b.w - d.i.w)) {
                if (lastLength == 0) {
                    textToRender = "...";
                    break;
                }

                var left = Math.floor(lastLength / 2);
                var right = left + (lastLength % 2);
                textToRender = this.text.substring(0, left) + "..." + this.text.substring(this.text.length - right);
                textMetrics = ctx.measureText(textToRender);

                lastLength -= 1;
            }

            var y = this.getInsets().top + textMetrics.ascent;
            if (th.browser.WebKit) y += 1;  // strings are one pixel too high in Safari 4 and Webkit nightly

            ctx.fillText(textToRender, this.getInsets().left, y);
        }
    }
});

th.ExpandingInfoPanel = Class.define({
    type: "ExpandingInfoPanel",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            this._super(parms);
        },

        getMinimumRowHeight: function() {
            return 40;
        },

        getMinimumColumnWidth: function() {

        },

        layout: function() {
            if (this.children.length == 0) return;

            var d = this.d();


            var rows = Math.floor(Math.sqrt(this.children.length));
            var height = Math.floor(d.b.h / rows);
            while (height < this.getMinimumRowHeight() && rows > 1) {
                rows--;
                height = Math.floor(d.b.h / rows);
            }


            var perRow = Math.floor(this.children.length / rows);
            var remainder = this.children.length % rows;


            var currentChild = 0;
            var heightRemainder = d.b.h % rows;

            var currentY = 0;
            for (var i = 0; i < rows; i++) {
                var h = (i == rows - 1) ? height + heightRemainder : height;

                var cols = (remainder > 0) ? perRow + 1 : perRow;
                remainder--;

                var width = Math.floor(d.b.w / cols);
                var widthRemainder = d.b.w % cols;

                var currentX = 0;
                for (var z = 0; z < cols; z++) {
                    var w = (z == cols - 1) ? width + widthRemainder : width;
                    this.children[currentChild++].bounds = { x: currentX, y: currentY, width: w, height: h };
                    currentX += w;
                }
                currentY += h;
            }
        }
    }
});

th.List = Class.define({
    type: "List",

    superclass: th.Container,

    members: {
        init: function(parms) {
            if (!parms) parms = {};
            this._super(parms);

            this.items = parms.items || [];

            this.scrollTop = 0;

            this.allowDeselection = parms.allowDeselection || false;

            this.bus.bind("mousedown", this, this.onmousedown, this);

            this.renderer = new th.Label();
            this.renderer.addCss("visibility", "hidden");    // prevent Th from rendering the label; we'll do it ourselves
            this.add(this.renderer);

            if (parms.topLabel) this.addTopLabel(parms.topLabel);
        },

        addTopLabel: function(label) {
            this.label = label;
            this.label.className = "header";
            this.add(this.label);
        },

        onmousedown: function(e) {
            var item = this.getItemForPosition({ x: e.componentX, y: e.componentY });
            if (item != this.selected) {
                if (item) {
                    this.selected = item;
                    this.bus.fire("itemselected", { container: this, item: this.selected }, this);
                    this.repaint();
                } else if (this.allowDeselection)  {
                    delete this.selected;
                }
            }
        },

        selectItemByText: function(text) {
            if (this.items.length == 0) return false;
            var item = null;
            if (th.isObject(this.items[0])) {
                for (var x = 0; x < this.items.length; x++) {
                    if (this.items[x].name == text) {
                        item = this.items[x];
                        break;
                    }
                }
                if (item == null)    return false;
            } else {
                if (this.items.indexOf(text) == -1)   return false;
                item = this.items[this.items.indexOf(text)];
            }

            if (this.selected != item) {
                this.selected = item;
                this.repaint();
            }

            return true;
        },

        moveSelectionUp: function() {
            if (!this.selected || this.items.length == 0) return;

            var x = 0;
            while (this.items[x] != this.selected) {
                x ++;
            }

            if (x != 0) {
                this.selected = this.items[x - 1];
                this.bus.fire("itemselected", { container: this, item: this.selected }, this);
                this.repaint();
            }
        },

        moveSelectionDown: function() {
            if (!this.selected || this.items.length == 0) return;

            var x = 0;
            while (this.items[x] != this.selected) {
                x ++;
            }

            if (x != this.items.length - 1) {
                this.selected = this.items[x + 1];
                this.bus.fire("itemselected", { container: this, item: this.selected }, this);
                this.repaint();
            }
        },

        getItemForPosition: function(pos) {
            pos.y += this.scrollTop;
            var y = this.getInsets().top;
            if (this.label) y += this.label.bounds.height;
            for (var i = 0; i < this.items.length; i++) {
                var h = this.heights[i];
                if (pos.y >= y && pos.y <= y + h) return this.items[i];
                y += h;
            }
        },

        getRenderer: function(rctx) {
            this.renderer.text = rctx.item.toString();
            this.renderer.selected = rctx.selected;
            this.renderer.pseudoClass = (this.renderer.selected) ? "active" : undefined;
            this.renderer.item = rctx.item;
            this.renderer.className = rctx.even ? "even" : "odd";

            if (rctx.item.contents) this.renderer.className += "more";

            this.renderer.resolveCss();

            return this.renderer;
        },

        getRenderContext: function(item, row) {
            return { item: item, even: row % 2 == 0, selected: this.selected == item };
        },

        getRowHeight: function() {
            if (!this.rowHeight) {
                var d = this.d();
                var firstItem = (this.items.length > 0) ? this.items[0] : undefined;
                if (firstItem) {
                    var renderer = this.getRenderer(this.getRenderContext(firstItem, 0));

                    this.rowHeight = renderer.getPreferredSize().height;
                }
            }
            return this.rowHeight || 0;
        },

        getScrollInfo: function() {
            return { scrollTop: this.scrollTop, scrollHeight: this.getRowHeight() * this.items.length }
        },

        layout: function() {},

        paint: function(ctx) {
            var d = this.d();
            var scrollInfo = this.getScrollInfo();
            var paintHeight = Math.max(scrollInfo.scrollHeight, d.b.h);

            var y = d.i.t;
            if (this.label) {
                this.label.addCss("visibility", "visible");

                var prefHeight = this.label.getPreferredSize().height;
                this.label.bounds = { y: y, x: d.i.l, height: prefHeight, width: d.b.w };
                this.label.paint(ctx);
                this.label.border.paint(ctx);
                y += prefHeight;

                this.label.addCss("visibility", "hidden");   // prevent Th from rendering the label; we'll do it ourselves
            }

            ctx.save();
            ctx.translate(0, -this.scrollTop);

            try {
                this.paintBackground(ctx, 0, y, d.b.w, paintHeight);

                if (this.items.length == 0) return;

                if (!this.renderer) {
                    console.log("No renderer for List of type " + this.type + " with id " + this.id + "; cannot paint contents");
                    return;
                }


                this.heights = [];
                var itemCounter = 0;
                while (y < paintHeight) {   // stop painting if current label is below the current viewing region
                    var useRealItem = (itemCounter < this.items.length);
                    var item = useRealItem ? this.items[itemCounter] : "";

                    var stamp = this.getRenderer(this.getRenderContext(item, itemCounter));
                    if (!stamp) break;

                    var w = d.b.w - d.i.w;
                    var h = (this.rowHeight) ? this.rowHeight :
                            (useRealItem) ? stamp.getPreferredSize().height : this.heights[this.heights.length - 1];
                    if (useRealItem) this.heights.push(h);

                    if ((y + h) >= this.scrollTop) {    // only paint the label if it isn't above the current viewing region
                        stamp.bounds = { x: 0, y: 0, height: h, width: w };

                        ctx.save();
                        ctx.translate(d.i.l, y);
                        ctx.beginPath();
                        ctx.rect(0, 0, w, h);
                        ctx.closePath();
                        ctx.clip();

                        stamp.addCss("visibility", "visible");

                        stamp.paint(ctx);
                        stamp.border.paint(ctx);

                        stamp.addCss("visibility", "hidden");

                        ctx.restore();
                    }

                    itemCounter++;
                    y+= h;
                }
            } finally {
                ctx.restore();
            }
        }
    }
});

th.HorizontalTree = Class.define({
    type: "HorizontalTree",

    superclass: th.Panel,

    members: {
        init: function(parms) {
            if (!parms) parms = {};
            this._super(parms);

            this.orientation = th.HORIZONTAL;

            this.lists = [];
            this.splitters = [];
            this.listWidths = [];
        },

        setData: function(data) {
            for (var i = 0; i < this.lists.length; i++) {
                this.remove(this.lists[i]);
                this.remove(this.splitters[i]);
                this.bus.unbind(this.lists[i]);
                this.bus.unbind(this.splitters[i]);
            }
            this.lists = [];
            this.splitters = [];

            this.data = data;
            this.showChildren(null, data);
        },

        ondragstart: function(e) {
            var splitterIndex = this.splitters.indexOf(e.thComponent);
            this.startSize = this.listWidths[splitterIndex];
        },

        ondrag: function(e) {
            var splitterIndex = this.splitters.indexOf(e.thComponent);
            var delta = (this.orientation == th.HORIZONTAL) ? e.currentPos.x - e.startPos.x : e.currentPos.y - e.startPos.y;
            this.listWidths[splitterIndex] = this.startSize + delta;
            this.render();
        },

        ondragstop: function(e) {
            delete this.startSize;
        },

        updateData: function(parent, contents) {
            parent.contents = contents;
            if (this.getSelectedItem() == parent) {
                this.showChildren(parent, parent.contents);
            }
        },

        replaceList: function(index, contents) {
            this.lists[index].items = contents;
            delete this.lists[index].selected;
            this.render();
        },

        removeListsFrom: function(index) {
            for (var x = index; x < this.lists.length; x++) {
                this.bus.unbind(this.lists[x]);
                this.bus.unbind(this.splitters[x]);

                this.remove(this.lists[x]);
                this.remove(this.splitters[x]);
            }

            this.lists = this.lists.slice(0, index);
            this.splitters = this.splitters.slice(0, index);
        },

        showChildren: function(newItem, children) {
            if (this.details) {
                this.remove(this.details);
                delete this.details;
            }

            if (!th.isArray(children)) {
                children(this.getSelectedPath(), this);
                this.render();
                return;
            }

            if (!children || children.length == 0) return;
            var list = this.createList(children);
            if (this.id) list.id = this.id + "-list-" + (this.lists.length + 1);

            this.bus.bind("itemselected", list, this.itemSelected, this);
            var tree = this;
            this.bus.bind("dblclick", list, function(e) {
                tree.bus.fire("dblclick", e, tree);
            });
            this.lists.push(list);
            this.add(list);

            var splitter = new th.Splitter({ orientation: th.HORIZONTAL , scrollbar: new th.Scrollbar() });
            splitter.scrollbar.scrollable = list;
            splitter.scrollbar.opaque = false;
            this.bus.bind("dragstart", splitter, this.ondragstart, this);
            this.bus.bind("drag", splitter, this.ondrag, this);
            this.bus.bind("dragstop", splitter, this.ondragstop, this);

            this.splitters.push(splitter);
            this.add(splitter);

            if (this.parent) this.render();
        },

        showDetails: function(item) {
            if (this.details) this.remove(this.details);


            if (this.parent) this.repaint();
        },

        createList: function(items) {
            var list = new th.List({ items: items });
            if (this.renderer) list.renderer = this.renderer;
            list.oldGetRenderer = list.getRenderer;
            list.getRenderer = function(rctx) {
                var label = list.oldGetRenderer(rctx);
                label.text = rctx.item.name;
                return label;
            }
            return list;
        },

        getSelectedItem: function() {
            var selected = this.getSelectedPath();
            if (selected.length > 0) return selected[selected.length - 1];
        },

        getSelectedPath: function(asString) {
            asString = asString || false;
            var path = [];

            for (var i = 0; i < this.lists.length; i++) {
                if (this.lists[i].selected) {
                    path.push(this.lists[i].selected);
                } else {
                    break;
                }
            }

            if (path.length == 0) return;

            if (asString) {
                var result = '';
                for (var i = 0; i < path.length - 1; i++) {
                    result += path[i].name + '/';
                }
                if (!path[path.length - 1].contents) {
                    result += path[path.length - 1].name
                } else {
                    result += path[path.length - 1].name + '/';
                }

                return result;
            } else {
                return path;
            }

            return path;
        },

        itemSelected: function(e) {
            var list = e.thComponent;

            if (!list.selected) return;

            var path = [];

            for (var i = 0; i < this.lists.length; i++) {
                path.push(this.lists[i].selected);
                if (this.lists[i] == list) {
                    for (var j = i + 1; j < this.lists.length && this.lists[j].selected; j++) {
                        this.lists[j - 1].selected.lastSelected = this.lists[j].selected.name
                        delete this.lists[j].selected;
                    }
                    break;
                }
            }

            this.bus.fire('itemselected', {e: e}, this);

            if (path.length < this.lists.length) {
                var newlists = this.lists.slice(0, path.length);
                var newsplitters = this.splitters.slice(0, path.length);
                for (var z = path.length; z < this.lists.length; z++) {
                    this.bus.unbind(this.lists[z]);
                    this.bus.unbind(this.splitters[z]);

                    this.remove(this.lists[z]);
                    this.remove(this.splitters[z]);
                }
                this.lists = newlists;
                this.splitters = newsplitters;
            }

            var newItem = path[path.length-1];
            if (newItem && newItem.contents) {
                this.showChildren(newItem, newItem.contents);
            } else {
                this.showDetails(newItem);
            }
        },

        getItem: function(pathToItem) {
            var items = this.data;
            var item;
            for (var i = 0; i < pathToItem.length; i++) {
                for (var z = 0; z < items.length; z++) {
                    if (items[z] == pathToItem[i]) {
                        item = items[z];
                        items = item.contents;
                        break;
                    }
                }
            }
            return item;
        },

        layout: function() {
            var d = this.d();

            var x = d.i.l;
            for (var i = 0; i < this.lists.length; i++) {
                var list = this.lists[i];
                if (!this.listWidths) this.listWidths = [];
                if (!this.listWidths[i]) this.listWidths[i] = th.convertLengthToPixels(this.cssValue("-th-list-width"), this);
                var w = this.listWidths[i];
                list.bounds = { x: x, y: d.i.t, width: w, height: d.b.h - d.i.h };

                x += w;

                var splitter = this.splitters[i];
                w = splitter.getPreferredSize().width;
                splitter.bounds = { x: x, y: d.i.t, width: w, height: d.b.h - d.i.h };
                x += w;

            }

            if (this.details) {
                this.details.bounds = { x: x, y: d.i.t, width: th.convertLengthToPixels(this.cssValue("-th-default-width"), this), height: d.b.h - d.i.h };
            }
        },

        paintSelf: function(ctx) {
            this._super(ctx);
        }
    }
});
th.DEFAULT_CSS = "#root {/* this should probably be the browser's default font */font: 10pt Arial, sans-serif;}Scrollbar {/*-th-vertical-top-image: url(/images/dash_vscroll_track_top.png);*//*-th-vertical-middle-image: url(/images/dash_vscroll_track_middle.png);*//*-th-vertical-bottom-image: url(/images/dash_vscroll_track_bottom.png);*/-th-vertical-top-image: url(/images/scroll_gutter_top.png);-th-vertical-middle-image: url(/images/scroll_gutter_mid.png);-th-vertical-bottom-image: url(/images/scroll_gutter_btm.png);}Scrollbar > Button.bar {-th-top-image: url(/images/scroll_thumb_top.png);-th-middle-image: url(/images/scroll_thumb_mid.png);-th-bottom-image: url(/images/scroll_thumb_btm.png);}Scrollbar > Button.up {background-image: url(/images/scroll_cntrl_up.png);}Scrollbar > Button.down {background-image: url(/images/scroll_cntrl_dwn.png);}ResizeNib {-th-vertical-bar-color: rgb(10, 10, 8);-th-vertical-bar-shadow-color: rgb(185, 180, 158);-th-horizontal-bar-subtle-shadow-color: rgba(0, 0, 0, 0.1);-th-horizontal-bar-shadow-color: black;-th-horizontal-bar-color: rgb(183, 180, 160);}Label {padding: 2px 5px;}HorizontalTree {-th-list-width: 160px;-th-details-width: 150px;}";
/**
 * Copyright 2009 Tim Down.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * jshashtable
 *
 * jshashtable is a JavaScript implementation of a hash table. It creates a
 * single constructor function called Hashtable in the global scope.
 *
 * Author: Tim Down <tim@timdown.co.uk>
 * Version: 1.0
 * Build date: 5 February 2009
 * Website: http://www.timdown.co.uk/jshashtable
 */

if (typeof th == "undefined") th = {};

th.Hashtable = (function() {
	function isUndefined(obj) {
		return (typeof obj === "undefined");
	}

	function isFunction(obj) {
		return (typeof obj === "function");
	}

	function isString(obj) {
		return (typeof obj === "string");
	}

	function hasMethod(obj, methodName) {
		return isFunction(obj[methodName]);
	}

	function hasEquals(obj) {
		return hasMethod(obj, "equals");
	}

	function hasHashCode(obj) {
		return hasMethod(obj, "hashCode");
	}

	function keyForObject(obj) {
		if (isString(obj)) {
			return obj;
		} else if (hasHashCode(obj)) {
			var hashCode = obj.hashCode();
			if (!isString(hashCode)) {
				return keyForObject(hashCode);
			}
			return hashCode;
		} else if (hasMethod(obj, "toString")) {
			return obj.toString();
		} else {
			return String(obj);
		}
	}

	function equals_fixedValueHasEquals(fixedValue, variableValue) {
		return fixedValue.equals(variableValue);
	}

	function equals_fixedValueNoEquals(fixedValue, variableValue) {
		if (hasEquals(variableValue)) {
			return variableValue.equals(fixedValue);
		} else {
			return fixedValue === variableValue;
		}
	}

	function equals_equivalence(o1, o2) {
		return o1 === o2;
	}

	function arraySearch(arr, value, arrayValueFunction, returnFoundItem, equalityFunction) {
		var currentValue;
		for (var i = 0, len = arr.length; i < len; i++) {
			currentValue = arr[i];
			if (equalityFunction(value, arrayValueFunction(currentValue))) {
				return returnFoundItem ? [i, currentValue] : true;
			}
		}
		return false;
	}

	function arrayRemoveAt(arr, idx) {
		if (hasMethod(arr, "splice")) {
			arr.splice(idx, 1);
		} else {
			if (idx === arr.length - 1) {
				arr.length = idx;
			} else {
				var itemsAfterDeleted = arr.slice(idx + 1);
				arr.length = idx;
				for (var i = 0, len = itemsAfterDeleted.length; i < len; i++) {
					arr[idx + i] = itemsAfterDeleted[i];
				}
			}
		}
	}

	function checkKeyOrValue(kv, kvStr) {
		if (kv === null) {
			throw new Error("null is not a valid " + kvStr);
		} else if (isUndefined(kv)) {
			throw new Error(kvStr + " must not be undefined");
		}
	}

	var keyStr = "key", valueStr = "value";

	function checkKey(key) {
		checkKeyOrValue(key, keyStr);
	}

	function checkValue(value) {
		checkKeyOrValue(value, valueStr);
	}

	/*------------------------------------------------------------------------*/

	function Bucket(firstKey, firstValue, equalityFunction) {
		this.entries = [];
		this.addEntry(firstKey, firstValue);

		if (equalityFunction !== null) {
			this.getEqualityFunction = function() {
				return equalityFunction;
			};
		}
	}

	function getBucketEntryKey(entry) {
		return entry[0];
	}

	function getBucketEntryValue(entry) {
		return entry[1];
	}

	Bucket.prototype = {
		getEqualityFunction: function(searchValue) {
			if (hasEquals(searchValue)) {
				return equals_fixedValueHasEquals;
			} else {
				return equals_fixedValueNoEquals;
			}
		},

		searchForEntry: function(key) {
			return arraySearch(this.entries, key, getBucketEntryKey, true, this.getEqualityFunction(key));
		},

		getEntryForKey: function(key) {
			return this.searchForEntry(key)[1];
		},

		getEntryIndexForKey: function(key) {
			return this.searchForEntry(key)[0];
		},

		removeEntryForKey: function(key) {
			var result = this.searchForEntry(key);
			if (result) {
				arrayRemoveAt(this.entries, result[0]);
				return true;
			}
			return false;
		},

		addEntry: function(key, value) {
			this.entries[this.entries.length] = [key, value];
		},

		size: function() {
			return this.entries.length;
		},

		keys: function(keys) {
			var startIndex = keys.length;
			for (var i = 0, len = this.entries.length; i < len; i++) {
				keys[startIndex + i] = this.entries[i][0];
			}
		},

		values: function(values) {
			var startIndex = values.length;
			for (var i = 0, len = this.entries.length; i < len; i++) {
				values[startIndex + i] = this.entries[i][1];
			}
		},

		containsKey: function(key) {
			return arraySearch(this.entries, key, getBucketEntryKey, false, this.getEqualityFunction(key));
		},

		containsValue: function(value) {
			return arraySearch(this.entries, value, getBucketEntryValue, false, equals_equivalence);
		}
	};

	/*------------------------------------------------------------------------*/

	function BucketItem() {}
	BucketItem.prototype = [];


	function getBucketKeyFromBucketItem(bucketItem) {
		return bucketItem[0];
	}

	function searchBucketItems(bucketItems, bucketKey, equalityFunction) {
		return arraySearch(bucketItems, bucketKey, getBucketKeyFromBucketItem, true, equalityFunction);
	}

	function getBucketForBucketKey(bucketItemsByBucketKey, bucketKey) {
		var bucketItem = bucketItemsByBucketKey[bucketKey];

		if (bucketItem && (bucketItem instanceof BucketItem)) {
			return bucketItem[1];
		}
		return null;
	}

	/*------------------------------------------------------------------------*/

	function Hashtable(hashingFunction, equalityFunction) {
		var bucketItems = [];
		var bucketItemsByBucketKey = {};

		hashingFunction = isFunction(hashingFunction) ? hashingFunction : keyForObject;
		equalityFunction = isFunction(equalityFunction) ? equalityFunction : null;

		this.put = function(key, value) {
			checkKey(key);
			checkValue(value);
			var bucketKey = hashingFunction(key);

			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);
			if (bucket) {
				var bucketEntry = bucket.getEntryForKey(key);
				if (bucketEntry) {
					bucketEntry[1] = value;
				} else {
					bucket.addEntry(key, value);
				}
			} else {
				var bucketItem = new BucketItem();
				bucketItem[0] = bucketKey;
				bucketItem[1] = new Bucket(key, value, equalityFunction);
				bucketItems[bucketItems.length] = bucketItem;
				bucketItemsByBucketKey[bucketKey] = bucketItem;
			}
		};

		this.get = function(key) {
			checkKey(key);

			var bucketKey = hashingFunction(key);

			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);
			if (bucket) {
				var bucketEntry = bucket.getEntryForKey(key);
				if (bucketEntry) {
					return bucketEntry[1];
				}
			}
			return null;
		};

		this.containsKey = function(key) {
			checkKey(key);

			var bucketKey = hashingFunction(key);

			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);
			if (bucket) {
				return bucket.containsKey(key);
			}

			return false;
		};

		this.containsValue = function(value) {
			checkValue(value);
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				if (bucketItems[i][1].containsValue(value)) {
					return true;
				}
			}
			return false;
		};

		this.clear = function() {
			bucketItems.length = 0;
			bucketItemsByBucketKey = {};
		};

		this.isEmpty = function() {
			return bucketItems.length === 0;
		};

		this.keys = function() {
			var keys = [];
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				bucketItems[i][1].keys(keys);
			}
			return keys;
		};

		this.values = function() {
			var values = [];
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				bucketItems[i][1].values(values);
			}
			return values;
		};

		this.remove = function(key) {
			checkKey(key);

			var bucketKey = hashingFunction(key);

			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);

			if (bucket) {
				if (bucket.removeEntryForKey(key)) {
					if (bucket.size() === 0) {
						var result = searchBucketItems(bucketItems, bucketKey, bucket.getEqualityFunction(key));
						arrayRemoveAt(bucketItems, result[0]);
						delete bucketItemsByBucketKey[bucketKey];
					}
				}
			}
		};

		this.size = function() {
			var total = 0;
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				total += bucketItems[i][1].size();
			}
			return total;
		};
	}

	return Hashtable;
})();
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is JGoodies FormLayout.
 *
 * The Initial Developer of the Original Code is JGoodies Karsten Lentzsch.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 *             The BSD License for the JGoodies Forms
 *             ======================================
 *
 * Copyright (c) 2002-2008 JGoodies Karsten Lentzsch. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * o Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * o Neither the name of JGoodies Karsten Lentzsch nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ***** END LICENSE BLOCK ***** */

if (typeof th == "undefined") th = {};

/*
 * This code is part of a partial port of JGoodies Karsten Lentzsch's excellent JGoodies FormLayout, a layout manager for
 * Java Swing. For more information, see http://www.jgoodies.com/freeware/forms/.
 *
 * NOTES:
 * - Most assert-style exception guards (e.g., null checks) have been removed
 *
 * - Defensive copying (e.g., clone, deepClone) has been removed
 *
 * - Any comments inside of methods were returned (pretty much none were present in original source); JavaDoc comments were not.
 *   See the original source for these; should apply 1:1 modulo standard Java<->JavaScript stuff
 *
 * - It was literally painful to include Java-style getters and setters, but I did so to avoid having to replace all invocations
 *   of these methods as I ported; e.g., I wasn't sure how often a subclass might override a getter/setter to provide extra
 *   functionality. These should be removed in a second pass.
 *
 * - I haven't implemented the string-based layout DSL yet; the following classes have omitted methods which need to be added
 *   to support this feature (may not be a complete list):
 *   - FormSpec, ColumnSpec
 *
 * - The following methods required adjustment due to Java overloading:
 *   - FormLayout.setHonorsVisibility(Component, Boolean) => setHonorsVisibilityForComponent(component, boolean)
 *   - FormSpec(DefaultAlignment, String) => omitted
 *   - ColumnSpec(Size) => omitted
 *   - ColumnSpec(EncodedDescription) => omitted; use ColumnSpec.decode(string)
 *   - CellConstraints(*) => use the string variant
 *   - RowSpec(EncodedDescription) => omitted; use RowSpec.decode(string)
 *
 * - I've removed anything to do with serialization
 *
 * - I eliminated these methods because JS' lack of packaging / namespaces made them unnecessary:
 *   - Sizes.dluX(int)
 *   - Sizes.dluY(int)
 *
 * - LayoutMap is not yet supported
 *
 * - FormLayout(String, String) => FormLayout.fromStrings(string, string)
 *
 * - FormLayout.layoutContainer(container) => FormLayout.layout(container)
 *
 * - xxxLayoutSize => getXXXSize() (e.g., minimumLayoutSize => getMinimumSize())
 */


if (typeof th.formlayout == "undefined") th.formlayout = {};

th.formlayout.Exception = Class.define({
    members: {
        init: function(message) {
            this.message = message;
        },

        toString: function() {
            return this.message;
        }
    }
});

th.formlayout.IllegalArgumentException = Class.define({
    superclass: th.formlayout.Exception,

    members: {
        init: function(message) {
            this._super(message);
        }
    }
});

th.formlayout.IndexOutOfBoundsException = Class.define({
    superclass: th.formlayout.Exception,

    members: {
        init: function(message) {
            this._super(message);
        }
    }
});

th.formlayout.FormLayoutParseException = Class.define({
    superclass: th.formlayout.Exception,

    members: {
        init: function(message) {
            this._super(message);
        }
    }
});

th.formlayout.Unit = Class.define({
    members: {
        init: function(name, abbreviation, parseAbbreviation, requiresIntegers) {
            this.name = name;
            this.abbreviation = abbreviation;
            this.parseAbbreviation = parseAbbreviation;
            this.requiresIntegers = requiresIntegers;
        },

        toString: function() {
            return this.name;
        },

        encode: function() {
            return this.parseAbbreviation || this.abbreviation;
        },

        abbreviation: function() {
            return this.abbreviation;
        }
    }
});

th.formlayout.Unit.valueOf = function(name, horizontal) {
    if (name.length == 0) {
        var defaultUnit = th.formlayout.Sizes.getDefaultUnit();
        if (defaultUnit) {
            return defaultUnit;
        }
        return horizontal ? th.formlayout.ConstantSize.DIALOG_UNITS_X : th.formlayout.ConstantSize.DIALOG_UNITS_Y;
    } else if (name == "px")
        return th.formlayout.ConstantSize.PIXEL;
    else if (name == "dlu")
        return horizontal ? th.formlayout.ConstantSize.DIALOG_UNITS_X : th.formlayout.ConstantSize.DIALOG_UNITS_Y;
    else if (name == "em")
        return th.formlayout.ConstantSize.EM;
    else if (name == "pt")
        return th.formlayout.ConstantSize.POINT;
    else if (name == "in")
        return th.formlayout.ConstantSize.INCH;
    else if (name == "mm")
        return th.formlayout.ConstantSize.MILLIMETER;
    else if (name == "cm")
        return th.formlayout.ConstantSize.CENTIMETER;
    else
        throw "Invalid unit name '" + name + "'. Must be one of: " +
            "px, dlu, pt, mm, cm, in";
}

th.formlayout.FormLayout = Class.define({
    members: {
        init: function(colSpecs, rowSpecs) {
            if (th.isString(colSpecs)) colSpecs = th.formlayout.ColumnSpec.decodeSpecs(colSpecs);
            if (th.isString(rowSpecs)) rowSpecs = th.formlayout.RowSpec.decodeSpecs(rowSpecs);

            this.colSpecs = colSpecs;
            this.rowSpecs = rowSpecs;

            this.colGroupIndices = [];
            this.rowGroupIndices = [];

            this.constraintMap = new th.Hashtable();
            this.componentSizeCache = new th.formlayout.ComponentSizeCache();

            var self = this;
            this.minimumWidthMeasure = function(c) {
                return self.componentSizeCache.getMinimumSize(c).width;
            };
            this.minimumHeightMeasure = function(c) {
                return self.componentSizeCache.getMinimumSize(c).height;
            };
            this.preferredWidthMeasure = function(c) {
                return self.componentSizeCache.getPreferredSize(c).width;
            };
            this.preferredHeightMeasure = function(c) {
                return self.componentSizeCache.getPreferredSize(c).height;
            };
        },

        getColumnCount: function() {
            return this.colSpecs.length;
        },

        getColumnSpec: function(columnIndex) {
            return this.colSpecs[columnIndex - 1];
        },

        setColumnSpec: function(columnIndex, columnSpec) {
            this.colSpecs[columnIndex - 1] = columnSpec;
        },

        appendColumn: function(columnSpec) {
            this.colSpecs.push(columnSpec);
        },

        insertColumn: function(columnIndex, columnSpec) {
            if (columnIndex < 1 || columnIndex > this.getColumnCount()) {
                throw "The column index " + columnIndex +
                      "must be in the range [1, " + this.getColumnCount() + "].";
            }
            this.colSpecs[columnIndex - 1] =  columnSpec;
            this.shiftComponentsHorizontally(columnIndex, false);
            this.adjustGroupIndices(this.colGroupIndices, columnIndex, false);
        },

        removeColumn: function(columnIndex) {
            if (columnIndex < 1 || columnIndex > this.getColumnCount()) {
                throw "The column index " + columnIndex + " must be in the range [1, " + this.getColumnCount() + "].";
            }
            this.colSpecs.remove(columnIndex - 1);
            this.shiftComponentsHorizontally(columnIndex, true);
            this.adjustGroupIndices(this.colGroupIndices, columnIndex, true);
        },

        getRowCount: function() {
            return this.rowSpecs.length;
        },

        getRowSpec: function(rowIndex) {
            return this.rowSpecs[rowIndex - 1];
        },

        setRowSpec: function(rowIndex, rowSpec) {
            this.rowSpecs[rowIndex - 1] = rowSpec;
        },

        appendRow: function(rowSpec) {
            this.rowSpecs.push(rowSpec);
        },

        insertRow: function(rowIndex, rowSpec) {
            if (rowIndex < 1 || rowIndex > this.getRowCount()) {
                throw "The row index " + rowIndex +
                      " must be in the range [1, " + this.getRowCount() + "].";
            }
            this.rowSpecs.add(rowIndex - 1, rowSpec);
            this.shiftComponentsVertically(rowIndex, false);
            this.adjustGroupIndices(this.rowGroupIndices, rowIndex, false);
        },

        removeRow: function(rowIndex) {
            if (rowIndex < 1 || rowIndex > this.getRowCount()) {
                throw "The row index " + rowIndex +
                      "must be in the range [1, " + this.getRowCount() + "].";
            }
            this.rowSpecs.remove(rowIndex - 1);
            this.shiftComponentsVertically(rowIndex, true);
            this.adjustGroupIndices(this.rowGroupIndices, rowIndex, true);
        },

        shiftComponentsHorizontally: function(columnIndex, remove) {
            var offset = remove ? -1 : 1;

            var keys = this.constraintMap.keys();
            th.forEach(keys, function(key) {
                var constraints = this.constraintMap.get(key);
                var x1 = constraints.gridX;
                var w  = constraints.gridWidth;
                var x2 = x1 + w - 1;
                if (x1 == columnIndex && remove) {
                    throw "The removed column " + columnIndex +
                        " must not contain component origins.\n" +
                        "Illegal component=" + key;
                } else if (x1 >= columnIndex) {
                    constraints.gridX += offset;
                } else if (x2 >= columnIndex) {
                    constraints.gridWidth += offset;
                }
            });
        },

        shiftComponentsVertically: function(rowIndex, remove) {
            var offset = remove ? -1 : 1;
            var keys = this.constraintMap.keys();
            th.forEach(keys, function(key) {
                var constraints = this.constraintMap.get(key);
                var y1 = constraints.gridY;
                var h  = constraints.gridHeight;
                var y2 = y1 + h - 1;
                if (y1 == rowIndex && remove) {
                    throw "The removed row " + rowIndex +
                        " must not contain component origins.\n" +
                        "Illegal component=" + key;
                } else if (y1 >= rowIndex) {
                    constraints.gridY += offset;
                } else if (y2 >= rowIndex) {
                    constraints.gridHeight += offset;
                }
            });
        },

        adjustGroupIndices: function(allGroupIndices, modifiedIndex, remove) {
            var offset = remove ? -1 : +1;
            for (var group = 0; group < allGroupIndices.length; group++) {
                var groupIndices = allGroupIndices[group];
                for (var i = 0; i < groupIndices.length; i++) {
                    var index = groupIndices[i];
                    if (index == modifiedIndex && remove) {
                        throw "The removed index " + modifiedIndex + " must not be grouped.";
                    } else if (index >= modifiedIndex) {
                        groupIndices[i] += offset;
                    }
                }
            }
        },

        getConstraints: function(component) {
            return this.constraintMap.get(component);
        },

        setConstraints: function(component, constraints) {
            constraints.ensureValidGridBounds(this.getColumnCount(), this.getRowCount());
            this.constraintMap.put(component, constraints);
        },

        removeConstraints: function(component) {
            this.constraintMap.remove(component);
            this.componentSizeCache.removeEntry(component);
        },

        getColumnGroups: function() {
            return this.colGroupIndices;
        },

        setColumnGroups: function(colGroupIndices) {
            var maxColumn = this.getColumnCount();
            var usedIndices = [];
            for (var group = 0; group < this.colGroupIndices.length; group++) {
                for (var j = 0; j < this.colGroupIndices[group].length; j++) {
                    var colIndex = this.colGroupIndices[group][j];
                    if (colIndex < 1 || colIndex > maxColumn) {
                        throw "Invalid column group index " + colIndex +
                            " in group " + (group+1);
                    }
                    if (usedIndices[colIndex]) {
                        throw "Column index " + colIndex + " must not be used in multiple column groups.";
                    }
                    usedIndices[colIndex] = true;
                }
            }
            this.colGroupIndices = colGroupIndices;
        },

        addGroupedColumn: function(columnIndex) {
            var newColGroups = this.getColumnGroups();
            if (newColGroups.length == 0) {
                newColGroups = [0][ columnIndex ];
            } else {
                var lastGroupIndex = newColGroups.length - 1;
                var lastGroup = newColGroups[lastGroupIndex];
                var groupSize = lastGroup.length;
                var newLastGroup = lastGroup.slice(0, groupSize);
                newLastGroup[groupSize] = columnIndex;
                newColGroups[lastGroupIndex] = newLastGroup;
            }
            this.setColumnGroups(newColGroups);
        },

        getRowGroups: function() {
            return this.rowGroupIndices;
        },

        setRowGroups: function(rowGroupIndices) {
            var rowCount = this.getRowCount();
            var usedIndices = [];
            for (var i = 0; i < this.rowGroupIndices.length; i++) {
                for (var j = 0; j < this.rowGroupIndices[i].length; j++) {
                    var rowIndex = this.rowGroupIndices[i][j];
                    if (rowIndex < 1 || rowIndex > rowCount) {
                        throw "Invalid row group index " + rowIndex + " in group " + (i+1);
                    }
                    if (usedIndices[rowIndex]) {
                        throw "Row index " + rowIndex + " must not be used in multiple row groups.";
                    }
                    usedIndices[rowIndex] = true;
                }
            }
            this.rowGroupIndices = rowGroupIndices;
        },

        addGroupedRow: function(rowIndex) {
            var newRowGroups = this.getRowGroups();
            if (newRowGroups.length == 0) {
                newRowGroups = [0][ rowIndex ];
            } else {
                var lastGroupIndex = newRowGroups.length - 1;
                var lastGroup = newRowGroups[lastGroupIndex];
                var groupSize = lastGroup.length;
                var newLastGroup = lastGroup.slice(0, groupSize);
                newLastGroup[groupSize] = rowIndex;
                newRowGroups[lastGroupIndex] = newLastGroup;
            }
            this.setRowGroups(newRowGroups);
        },

        getHonorsVisibility: function() {
            return this.honorsVisibility;
        },

        setHonorsVisibility: function(b) {
            var oldHonorsVisibility = this.getHonorsVisibility();
            if (oldHonorsVisibility == b) return;
            this.honorsVisibility = b;

            var componentSet = this.constraintMap.keys();
            if (componentSet.length == 0) return;
            var firstComponent = componentSet[0];
            var container = firstComponent.parent;
            if (container) container.render();
        },

        setHonorsVisibilityForComponent: function(component, b) {
            var constraints = this.getConstraints(component);
            if (b == constraints.honorsVisibility) return;
            constraints.honorsVisibility = b;
            if (component.parent) component.parent.render();
        },

        addLayoutComponent: function(comp, constraints) {
            if (th.isString(constraints)) {
                this.setConstraints(comp, new th.formlayout.CellConstraints(constraints));
            } else {
                this.setConstraints(comp, constraints);
            }
        },

        removeLayoutComponent: function(comp) {
            this.removeConstraints(comp);
        },

        getMinimumSize: function(parent) {
            return this.computeLayoutSize(parent, this.minimumWidthMeasure, this.minimumHeightMeasure);
        },

        getPreferredSize: function(parent) {
            return this.computeLayoutSize(parent, this.preferredWidthMeasure, this.preferredHeightMeasure);
        },

        getMaximumSize: function(parent) {
            return { width: 1000000, height: 1000000 };
        },

        invalidateLayout: function(target) {
            this.invalidateCaches();
        },

        layout: function(/* Container */ parent) {
            this.initializeColAndRowComponentLists();
            var size = parent.getSize();

            var insets = parent.getInsets();
            var totalWidth  = size.width - insets.left - insets.right;
            var totalHeight = size.height- insets.top  - insets.bottom;

            var x = this.computeGridOrigins(parent,
                                         totalWidth, insets.left,
                                         this.colSpecs,
                                         this.colComponents,
                                         this.colGroupIndices,
                                         this.minimumWidthMeasure,
                                         this.preferredWidthMeasure
                                         );
            var y = this.computeGridOrigins(parent,
                                         totalHeight, insets.top,
                                         this.rowSpecs,
                                         this.rowComponents,
                                         this.rowGroupIndices,
                                         this.minimumHeightMeasure,
                                         this.preferredHeightMeasure
                                         );

            this.layoutComponents(x, y);
        },

        initializeColAndRowComponentLists: function() {
            this.colComponents = [];
            for (var i = 0; i < this.getColumnCount(); i++) {
                this.colComponents[i] = [];
            }

            this.rowComponents = [];
            for (var i = 0; i < this.getRowCount(); i++) {
                this.rowComponents[i] = [];
            }

            var keys = this.constraintMap.keys();
            th.forEach(keys, function(component) {
                var constraints = this.constraintMap.get(component);
                if (this.takeIntoAccount(component, constraints)) {
                    if (constraints.gridWidth == 1)
                        this.colComponents[constraints.gridX - 1].push(component);

                    if (constraints.gridHeight == 1)
                        this.rowComponents[constraints.gridY - 1].push(component);
                }
            }, this);
        },

        computeLayoutSize: function(parent, defaultWidthMeasure, defaultHeightMeasure) {
            this.initializeColAndRowComponentLists();
            var colWidths  = this.maximumSizes(parent, this.colSpecs, this.colComponents,
                                            this.minimumWidthMeasure,
                                            this.preferredWidthMeasure,
                                            defaultWidthMeasure);
            var rowHeights = this.maximumSizes(parent, this.rowSpecs, this.rowComponents,
                                            this.minimumHeightMeasure,
                                            this.preferredHeightMeasure,
                                            defaultHeightMeasure);
            var groupedWidths  = this.groupedSizes(this.colGroupIndices, colWidths);
            var groupedHeights = this.groupedSizes(this.rowGroupIndices, rowHeights);

            var xOrigins = this.computeOrigins(groupedWidths,  0);
            var yOrigins = this.computeOrigins(groupedHeights, 0);

            var width1  = this.sum(groupedWidths);
            var height1 = this.sum(groupedHeights);
            var maxWidth = width1;
            var maxHeight = height1;

            /*
             * Take components that span multiple columns or rows into account.
             * This shall be done if and only if a component spans an interval
             * that can grow.
             */
            var maxFixedSizeColsTable = this.computeMaximumFixedSpanTable(this.colSpecs);
            var maxFixedSizeRowsTable = this.computeMaximumFixedSpanTable(this.rowSpecs);

            var keys = this.constraintMap.keys();
            var self = this;
            th.forEach(keys, function(component) {
                var constraints = self.constraintMap.get(component);

                if (!self.takeIntoAccount(component, constraints)) return;

                if (   (constraints.gridWidth > 1)
                    && (constraints.gridWidth > maxFixedSizeColsTable[constraints.gridX-1])) {
                    var compWidth = defaultWidthMeasure(component);
                    var gridX1 = constraints.gridX-1;
                    var gridX2 = gridX1 + constraints.gridWidth;
                    var lead  = xOrigins[gridX1];
                    var trail = width1 - xOrigins[gridX2];
                    var myWidth = lead + compWidth + trail;
                    if (myWidth > maxWidth) {
                        maxWidth = myWidth;
                    }
                }

                if (   (constraints.gridHeight > 1)
                    && (constraints.gridHeight > maxFixedSizeRowsTable[constraints.gridY-1])) {
                    var compHeight = defaultHeightMeasure(component);
                    var gridY1 = constraints.gridY-1;
                    var gridY2 = gridY1 + constraints.gridHeight;
                    var lead  = yOrigins[gridY1];
                    var trail = height1 - yOrigins[gridY2];
                    var myHeight = lead + compHeight + trail;
                    if (myHeight > maxHeight) {
                        maxHeight = myHeight;
                    }
                }
            });
            var insets = parent.getInsets();
            var width  = maxWidth  + insets.left + insets.right;
            var height = maxHeight + insets.top  + insets.bottom;
            return { width: width, height: height };
        },

        computeGridOrigins: function(container, totalSize, offset, formSpecs, componentLists, groupIndices, minMeasure, prefMeasure) {
            /* For each spec compute the minimum and preferred size that is
             * the maximum of all component minimum and preferred sizes resp.
             */
            var minSizes   = this.maximumSizes(container, formSpecs, componentLists,
                                            minMeasure, prefMeasure, minMeasure);
            var prefSizes  = this.maximumSizes(container, formSpecs, componentLists,
                                            minMeasure, prefMeasure, prefMeasure);

            var groupedMinSizes  = this.groupedSizes(groupIndices, minSizes);
            var groupedPrefSizes = this.groupedSizes(groupIndices, prefSizes);
            var   totalMinSize     = this.sum(groupedMinSizes);
            var   totalPrefSize    = this.sum(groupedPrefSizes);
            var compressedSizes  = this.compressedSizes(formSpecs,
                                                   totalSize,
                                                   totalMinSize,
                                                   totalPrefSize,
                                                   groupedMinSizes,
                                                   prefSizes);
            var groupedSizes     = this.groupedSizes(groupIndices, compressedSizes);
            var   totalGroupedSize = this.sum(groupedSizes);
            var sizes            = this.distributedSizes(formSpecs,
                                                     totalSize,
                                                     totalGroupedSize,
                                                     groupedSizes);
            return this.computeOrigins(sizes, offset);
        },

        computeOrigins: function(sizes, offset) {
            var count = sizes.length;
            var origins = [];
            origins[0] = offset;
            for (var i = 1; i <= count; i++) {
                origins[i] = origins[i-1] + sizes[i-1];
            }
            return origins;
        },

        layoutComponents: function(x, y) {
            var keys = this.constraintMap.keys();
            th.forEach(keys, function(component) {
                var cellBounds = {};
                var constraints = this.constraintMap.get(component);
                var gridX      = constraints.gridX - 1;
                var gridY      = constraints.gridY - 1;
                var gridWidth  = constraints.gridWidth;
                var gridHeight = constraints.gridHeight;
                cellBounds.x = x[gridX];
                cellBounds.y = y[gridY];
                cellBounds.width  = x[gridX + gridWidth ] - cellBounds.x;
                cellBounds.height = y[gridY + gridHeight] - cellBounds.y;

                constraints.setBounds(component, this, cellBounds,
                                this.minimumWidthMeasure,   this.minimumHeightMeasure,
                                this.preferredWidthMeasure, this.preferredHeightMeasure);
            }, this);
        },

        invalidateCaches: function() {
            this.componentSizeCache.invalidate();
        },

        maximumSizes: function(container,
                                formSpecs,
                                componentLists,
                                minMeasure,
                                prefMeasure,
                                defaultMeasure) {
            var size = formSpecs.length;
            var result = [];
            for (var i = 0; i < size; i++) {
                var formSpec = formSpecs[i];
                result[i] = formSpec.maximumSize(container,
                                                 componentLists[i],
                                                 minMeasure,
                                                 prefMeasure,
                                                 defaultMeasure);
            }
            return result;
        },

        compressedSizes: function(formSpecs, totalSize, totalMinSize, totalPrefSize, minSizes, prefSizes) {

            if (totalSize < totalMinSize)
                return minSizes;
            if (totalSize >= totalPrefSize)
                return prefSizes;

            var count = formSpecs.length;
            var sizes = [];

            var totalCompressionSpace = totalPrefSize - totalSize;
            var maxCompressionSpace   = totalPrefSize - totalMinSize;
            var compressionFactor     = totalCompressionSpace / maxCompressionSpace;


            for (var i=0; i < count; i++) {
                var formSpec = formSpecs[i];
                sizes[i] = prefSizes[i];
                if (formSpec.getSize().compressible()) {
                    sizes[i] -= Math.round((prefSizes[i] - minSizes[i]) * compressionFactor);
                }
            }
            return sizes;
        },

        groupedSizes: function(groups, rawSizes) {
            if (!groups || groups.length == 0) {
                return rawSizes;
            }

            var sizes = [];
            for (var i = 0; i < sizes.length; i++) {
                sizes[i] = rawSizes[i];
            }

            for (var group = 0; group < groups.length; group++) {
                var groupIndices = groups[group];
                var groupMaxSize = 0;
                for (var i = 0; i < groupIndices.length; i++) {
                    var index = groupIndices[i] - 1;
                    groupMaxSize = Math.max(groupMaxSize, sizes[index]);
                }
                for (var i = 0; i < groupIndices.length; i++) {
                    var index = groupIndices[i] - 1;
                    sizes[index] = groupMaxSize;
                }
            }
            return sizes;
        },

        distributedSizes: function(formSpecs, totalSize, totalPrefSize, inputSizes) {
            var totalFreeSpace = totalSize - totalPrefSize;
            if (totalFreeSpace < 0)
                return inputSizes;

            var count = formSpecs.length;
            var totalWeight = 0.0;
            for (var i = 0; i < count; i++) {
                var formSpec = formSpecs[i];
                totalWeight += formSpec.getResizeWeight();
            }

            if (totalWeight == 0.0)
                return inputSizes;

            var sizes = [];

            var restSpace = totalFreeSpace;
            var roundedRestSpace = parseInt(totalFreeSpace);
            for (var i = 0; i < count; i++) {
                var formSpec = formSpecs[i];
                var weight = formSpec.getResizeWeight();
                if (weight == th.formlayout.FormSpec.NO_GROW) {
                    sizes[i] = inputSizes[i];
                } else {
                    var roundingCorrection = restSpace - roundedRestSpace;
                    var extraSpace = totalFreeSpace * weight / totalWeight;
                    var correctedExtraSpace = extraSpace - roundingCorrection;
                    var roundedExtraSpace = Math.round(correctedExtraSpace);
                    sizes[i] = inputSizes[i] + roundedExtraSpace;
                    restSpace -= extraSpace;
                    roundedRestSpace -= roundedExtraSpace;
                }
            }
            return sizes;
        },

        computeMaximumFixedSpanTable: function(formSpecs) {
            var size = formSpecs.length;
            var table = [];
            var maximumFixedSpan = 1000000;        // Could be 1
            for (var i = size-1; i >= 0; i--) {
                var spec = formSpecs[i];
                if (spec.canGrow()) {
                    maximumFixedSpan = 0;
                }
                table[i] = maximumFixedSpan;
                if (maximumFixedSpan < 1000000)
                    maximumFixedSpan++;
            }
            return table;
        },

        sum: function(sizes) {
            var sum = 0;
            for (var i = sizes.length - 1; i >= 0; i--) {
                sum += sizes[i];
            }
            return sum;
        },

        takeIntoAccount: function(component, cc) {
            return   component.cssValue("visibility") != "hidden"
                  || ((!cc.honorsVisibility) && !this.getHonorsVisibility())
                  || !cc.honorsVisibility;
        },

        getLayoutInfo: function (parent) {
            this.initializeColAndRowComponentLists();
            var size = parent.getSize();
            var insets = parent.getInsets();
            var totalWidth = size.width - insets.left - insets.right;
            var totalHeight = size.height - insets.top - insets.bottom;
            var x = this.computeGridOrigins(parent,
                    totalWidth, insets.left,
                    this.colSpecs,
                    this.colComponents,
                    this.colGroupIndices,
                    this.minimumWidthMeasure,
                    this.preferredWidthMeasure);
            var y = this.computeGridOrigins(parent,
                    totalHeight, insets.top,
                    this.rowSpecs,
                    this.rowComponents,
                    this.rowGroupIndices,
                    this.minimumHeightMeasure,
                    this.preferredHeightMeasure);
            return new th.formlayout.LayoutInfo(x, y);
        }
    }
});

th.formlayout.LayoutInfo = Class.define({
    members: {
        init: function(xOrigins, yOrigins) {
            this.columnOrigins = xOrigins;
            this.rowOrigins = yOrigins;
        },

        getX: function () {
            return this.columnOrigins[0];
        },

        getY: function () {
            return this.rowOrigins[0];
        },

        getWidth: function () {
            return this.columnOrigins[this.columnOrigins.length - 1] - this.columnOrigins[0];
        },

        getHeight: function () {
            return this.rowOrigins[this.rowOrigins.length - 1] - this.rowOrigins[0];
        }
    }
});


th.formlayout.ComponentSizeCache = Class.define({
    members: {
        init: function() {
            this.minimumSizes = new th.Hashtable();
            this.preferredSizes = new th.Hashtable();
        },

        invalidate: function() {
            this.minimumSizes.clear();
            this.preferredSizes.clear();
        },

        getSize: function(component, hash, sizeType) {
            var size = hash.get(component);
            if (!size) {
                size = component[sizeType]();
                hash.put(component, size);
            }
            return size;

        },

        getMinimumSize: function(component) {
            return this.getSize(component, this.minimumSizes, "getMinimumSize");
        },

        getPreferredSize: function(component) {
            return this.getSize(component, this.preferredSizes, "getPreferredSize");
        },

        removeEntry: function(component) {
            this.minimumSizes.remove(component);
            this.preferredSizes.remove(component);
        }
    }
});

th.formlayout.FormSpec = Class.define({
    members: {
        NO_GROW: 0.0,

        DEFAULT_GROW: 1.0,

        spacer: false,

        init: function(defaultAlignment, size, resizeWeight) {
            this.defaultAlignment = defaultAlignment;
            this.size = size;
            this.resizeWeight = resizeWeight || 0;
        },

        isSpacer: function() {
            return this.spacer;
        },

        getDefaultAlignment: function() {
            return this.defaultAlignment;
        },

        getSize: function() {
            return this.size;
        },

        getResizeWeight: function() {
            return this.resizeWeight;
        },

        canGrow: function() {
            return this.resizeWeight != this.NO_GROW;
        },

        isHorizontal: function() {
            throw "This needs to be overridden.";
        },

        setDefaultAlignment: function(defaultAlignment) {
            this.defaultAlignment = defaultAlignment;
        },

        setSize: function(size) {
            this.size = size;
        },

        setResizeWeight: function(resizeWeight) {
            this.resizeWeight = resizeWeight;
        },

        maximumSize: function(container,
                        components,
                        minMeasure,
                        prefMeasure,
                        defaultMeasure) {
            return this.size.maximumSize(container,
                                     components,
                                     minMeasure,
                                     prefMeasure,
                                     defaultMeasure);
        },

        parseAndInitValues: function(encodedDescription) {
            if (encodedDescription.indexOf("spacer(") == 0) {
                encodedDescription = encodedDescription.substring("spacer(".length, encodedDescription.length - 1);
                this.spacer = true;
            }

            var token = encodedDescription.split(":");
            if (token.length == 0) {
                throw new th.formlayout.IllegalArgumentException(
                                        "The form spec must not be empty.");
            }
            var nextIndex = 0;
            var next = token[nextIndex++];

            var alignment = th.formlayout.DefaultAlignment.valueOf(next, this.isHorizontal());
            if (alignment) {
                this.setDefaultAlignment(alignment);
                if (token.length == 1) {
                    throw new th.formlayout.IllegalArgumentException(
                                        "The form spec must provide a size.");
                }
                next = token[nextIndex++];
            }
            this.setSize(this.parseSize(next));
            if (nextIndex < token.length) {
                this.setResizeWeight(this.parseResizeWeight(token[nextIndex]));
            }

        },

        parseSize: function(token) {
            if (token.indexOf("[") == 0 && token.indexOf("]") == token.length - 1) {
                return this.parseBoundedSize(token);
            }
            if (token.indexOf("max(") == 0 && token.indexOf(")") == token.length - 1) {
                return this.parseOldBoundedSize(token, false);
            }
            if (token.indexOf("min(") == 0 && token.indexOf(")") == token.length - 1) {
                return this.parseOldBoundedSize(token, true);
            }
            return this.parseAtomicSize(token);
        },


        parseBoundedSize: function(token) {
            var content = token.substring(1, token.length - 1);
            var subtoken = content.split(/\s*,\s*/);
            var basis = undefined;
            var lower = undefined;
            var upper = undefined;
            if (subtoken.length == 2) {
                var size1 = this.parseAtomicSize(subtoken[0]);
                var size2 = this.parseAtomicSize(subtoken[1]);
                if (this.isConstant(size1)) {
                    if (this.isConstant(size2)) {
                        lower = size1;
                        basis = size2;
                        upper = size2;
                    } else {
                        lower = size1;
                        basis = size2;
                    }
                } else {
                    basis = size1;
                    upper = size2;
                }
            } else if (subtoken.length == 3) {
                lower = this.parseAtomicSize(subtoken[0]);
                basis = this.parseAtomicSize(subtoken[1]);
                upper = this.parseAtomicSize(subtoken[2]);
            }
            if (   ((lower == null) || (this.isConstant(lower)))
                && ((upper == null) || (this.isConstant(upper))))  {
                return new th.formlayout.BoundedSize(basis, lower, upper);
            }
            throw new th.formlayout.IllegalArgumentException(
                    "Illegal bounded size '" + token + "'. Must be one of:"
                  + "\n[<constant size>,<logical size>]                 // lower bound"
                  + "\n[<logical size>,<constant size>]                 // upper bound"
                  + "\n[<constant size>,<logical size>,<constant size>] // lower and upper bound."
                  + "\nExamples:"
                  + "\n[50dlu,pref]                                     // lower bound"
                  + "\n[pref,200dlu]                                    // upper bound"
                  + "\n[50dlu,pref,200dlu]                              // lower and upper bound."
                  );
        },

        parseAtomicSize: function(token) {
            var trimmedToken = th.trim(token);
            if (   trimmedToken.indexOf("'") == 0 && trimmedToken.indexOf("'") == trimmedToken.length - 1) {
                var length = trimmedToken.length;
                if (length < 2) {
                    throw new th.formlayout.IllegalArgumentException("Missing closing \"'\" for prototype.");
                }
                return new th.formlayout.PrototypeSize(trimmedToken.substring(1, length - 1));
            }
            var componentSize = th.formlayout.ComponentSize.valueOf(trimmedToken);
            if (componentSize != null)
                return componentSize;
            return th.formlayout.ConstantSize.valueOf(trimmedToken, this.isHorizontal());
        },

        parseResizeWeight: function(token) {
            if (token == "g" || token == "grow") {
                return th.formlayout.FormSpec.DEFAULT_GROW;
            }
            if (token == "n" || token == "nogrow" || token == "none") {
                return th.formlayout.FormSpec.NO_GROW;
            }
            if ((token.indexOf("grow(") == 0 || token.indexOf("g(") == 0)
                 && token.indexOf(")") == token.length - 1) {
                var leftParen  = token.indexOf('(');
                var rightParen = token.indexOf(')');
                var substring = token.substring(leftParen + 1, rightParen);
                return parseFloat(substring);
            }
            throw new th.formlayout.IllegalArgumentException(
                        "The resize argument '" + token + "' is invalid. " +
                        " Must be one of: grow, g, none, n, grow(<double>), g(<double>)");
        },

        isConstant: function(aSize) {
            return  (aSize instanceof th.formlayout.ConstantSize)
                 || (aSize instanceof th.formlayout.PrototypeSize);
        },

        toString: function() {
            buffer = this.defaultAlignment;

            buffer += ":";
            buffer += this.size.toString();
            buffer += ':';
            if (this.resizeWeight == th.formlayout.FormSpec.NO_GROW) {
                buffer += "noGrow";
            } else if (this.resizeWeight == th.formlayout.FormSpec.DEFAULT_GROW) {
                buffer += "grow";
            } else {
                buffer += "grow(";
                buffer += this.resizeWeight;
                buffer += ')';
            }
            return buffer;
        },

        toShortString: function() {
            buffer = this.defaultAlignment.abbreviation();

            buffer += ":";
            buffer += this.size.toString();
            buffer += ':';
            if (this.resizeWeight == th.formlayout.FormSpec.NO_GROW) {
                buffer += "n";
            } else if (this.resizeWeight == th.formlayout.FormSpec.DEFAULT_GROW) {
                buffer += "g";
            } else {
                buffer += "g(";
                buffer += this.resizeWeight;
                buffer += ')';
            }
            return buffer;
        },

        encode: function() {
            var buffer = "";
            var alignmentDefault = this.isHorizontal()
                ? th.formlayout.ColumnSpec.DEFAULT
                : th.formlayout.RowSpec.DEFAULT;
            if (!alignmentDefault.equals(this.defaultAlignment)) {
                buffer += this.defaultAlignment.abbreviation();
                buffer += ":";
            }
            buffer += this.size.encode();
            if (this.resizeWeight == th.formlayout.FormSpec.NO_GROW) {
            } else if (this.resizeWeight == th.formlayout.FormSpec.DEFAULT_GROW) {
                buffer += ':';
                buffer += "g";
            } else {
                buffer += ':';
                buffer += "g(";
                buffer += this.resizeWeight;
                buffer += ')';
            }
            return buffer;
        }
    }
});

th.formlayout.DefaultAlignment = Class.define({
    members: {
        init: function(name) {
            this.name = name;
        },

        toString: function() {
            return this.name;
        },

        abbreviation: function() {
            return this.name.charAt(0);
        }
    }
});

th.formlayout.DefaultAlignment.valueOf = function(str, isHorizontal) {
    if (str == "f" || str == "fill")
        return th.formlayout.FormSpec.FILL_ALIGN;
    else if (str == "c" || str == "center")
        return th.formlayout.FormSpec.CENTER_ALIGN;
    else if (isHorizontal) {
        if (str == "r" || str == "right")
            return th.formlayout.FormSpec.RIGHT_ALIGN;
        else if (str == "l" || str == "left")
            return th.formlayout.FormSpec.LEFT_ALIGN;
        else
            return;
    } else {
        if (str == "t" || str == "top")
            return th.formlayout.FormSpec.TOP_ALIGN;
        else if (str == "b" || str == "bottom")
            return th.formlayout.FormSpec.BOTTOM_ALIGN;
        else
            return;
    }
}

th.formlayout.FormSpec.LEFT_ALIGN = new th.formlayout.DefaultAlignment("left");
th.formlayout.FormSpec.RIGHT_ALIGN = new th.formlayout.DefaultAlignment("right");
th.formlayout.FormSpec.TOP_ALIGN = new th.formlayout.DefaultAlignment("top");
th.formlayout.FormSpec.BOTTOM_ALIGN = new th.formlayout.DefaultAlignment("bottom");
th.formlayout.FormSpec.CENTER_ALIGN = new th.formlayout.DefaultAlignment("center");
th.formlayout.FormSpec.FILL_ALIGN = new th.formlayout.DefaultAlignment("fill");

th.formlayout.ColumnSpec = Class.define({
    superclass: th.formlayout.FormSpec,

    members: {
        init: function(defaultAlignment, size, resizeWeight) {
            this._super(defaultAlignment, size, resizeWeight);
        },

        isHorizontal: function() {
            return true;
        }
    }
});

th.formlayout.ColumnSpec.decode = function(encodedColumnSpec, layoutMap) {
    var trimmed = th.trim(encodedColumnSpec);
    var lower = trimmed.toLowerCase();
    return th.formlayout.ColumnSpec.decodeExpanded(lower);
}

th.formlayout.ColumnSpec.decodeExpanded = function(expandedTrimmedLowerCaseSpec) {
    var spec = th.formlayout.ColumnSpec.CACHE.get(expandedTrimmedLowerCaseSpec);
    if (!spec) {
        spec = new th.formlayout.ColumnSpec(th.formlayout.ColumnSpec.DEFAULT, th.formlayout.Sizes.DEFAULT, th.formlayout.FormSpec.NO_GROW);
        spec.parseAndInitValues(expandedTrimmedLowerCaseSpec);
        th.formlayout.ColumnSpec.CACHE.put(expandedTrimmedLowerCaseSpec, spec);
    }
    return spec;
};

th.formlayout.ColumnSpec.decodeSpecs = function(encodedColumnSpecs, layoutMap) {
    return th.formlayout.FormSpecParser.parseColumnSpecs(encodedColumnSpecs, layoutMap);
}

th.formlayout.ColumnSpec.createGap = function(gapHeight) {
    return new th.formlayout.RowSpec(th.formlayout.ColumnSpec.DEFAULT, gapHeight, th.formlayout.ColumnSpec.NO_GROW);
}

th.formlayout.ColumnSpec.LEFT = th.formlayout.FormSpec.LEFT_ALIGN;
th.formlayout.ColumnSpec.CENTER = th.formlayout.FormSpec.CENTER_ALIGN;
th.formlayout.ColumnSpec.MIDDLE = th.formlayout.ColumnSpec.CENTER;
th.formlayout.ColumnSpec.RIGHT = th.formlayout.FormSpec.RIGHT_ALIGN;
th.formlayout.ColumnSpec.FILL = th.formlayout.FormSpec.FILL_ALIGN;
th.formlayout.ColumnSpec.DEFAULT = th.formlayout.ColumnSpec.FILL;

th.formlayout.ColumnSpec.CACHE = new th.Hashtable();

th.formlayout.RowSpec = Class.define({
    superclass: th.formlayout.FormSpec,

    members: {
        init: function(defaultAlignment, size, resizeWeight) {
            this._super(defaultAlignment, size, resizeWeight);
        },

        isHorizontal: function() {
            return false;
        }
    }
});

th.formlayout.RowSpec.decode = function(encodedRowSpec, layoutMap) {
    var trimmed = th.trim(encodedRowSpec);
    var lower = trimmed.toLowerCase();
    return th.formlayout.RowSpec.decodeExpanded(lower);
}

th.formlayout.RowSpec.decodeExpanded = function(expandedTrimmedLowerCaseSpec) {
    var spec = th.formlayout.RowSpec.CACHE.get(expandedTrimmedLowerCaseSpec);
    if (!spec) {
        spec = new th.formlayout.RowSpec(th.formlayout.RowSpec.DEFAULT, th.formlayout.Sizes.DEFAULT, th.formlayout.FormSpec.NO_GROW);
        spec.parseAndInitValues(expandedTrimmedLowerCaseSpec);
        th.formlayout.RowSpec.CACHE.put(expandedTrimmedLowerCaseSpec, spec);
    }
    return spec;
};

th.formlayout.RowSpec.decodeSpecs = function(encodedRowSpecs, layoutMap) {
    return th.formlayout.FormSpecParser.parseRowSpecs(encodedRowSpecs, layoutMap);
}

th.formlayout.RowSpec.createGap = function(gapHeight) {
    return new th.formlayout.RowSpec(th.formlayout.RowSpec.DEFAULT, gapHeight, th.formlayout.RowSpec.NO_GROW);
}

th.formlayout.RowSpec.TOP = th.formlayout.FormSpec.TOP_ALIGN;
th.formlayout.RowSpec.CENTER = th.formlayout.FormSpec.CENTER_ALIGN;
th.formlayout.RowSpec.BOTTOM = th.formlayout.FormSpec.BOTTOM_ALIGN;
th.formlayout.RowSpec.FILL = th.formlayout.FormSpec.FILL_ALIGN;
th.formlayout.RowSpec.DEFAULT = th.formlayout.RowSpec.CENTER;

th.formlayout.RowSpec.CACHE = new th.Hashtable();

th.formlayout.BoundedSize = Class.define({
    members: {
        init: function(basis, lowerBound, upperBound) {
            this.basis = basis;
            this.lowerBound = lowerBound;
            this.upperBound = upperBound;
        },

        getBasis: function() {
            return this.basis;
        },

        getLowerBound: function() {
            return this.lowerBound;
        },

        getUpperBound: function() {
            return this.upperBound;
        },

        maximumSize: function(container, components, minMeasure, prefMeasure, defaultMeasure) {
            var size = this.basis.maximumSize(container, components, minMeasure, prefMeasure, defaultMeasure);

            if (this.lowerBound) {
                size = Math.max(size,
                                this.lowerBound.maximumSize(
                                        container,
                                        components,
                                        minMeasure,
                                        prefMeasure,
                                        defaultMeasure));
            }

            if (this.upperBound) {
                size = Math.min(size,
                                this.upperBound.maximumSize(
                                        container,
                                        components,
                                        minMeasure,
                                        prefMeasure,
                                        defaultMeasure));
            }

            return size;
        },

        compressible: function() {
            return this.basis.compressible();
        },

        equals: function(object) {
            if (this === object)
                return true;
            var size = object;
            return this.basis.equals(size.basis)
                 && (   (this.lowerBound == null && size.lowerBound == null)
                     || (this.lowerBound != null && this.lowerBound.equals(size.lowerBound)))
                 && (   (this.upperBound == null && size.upperBound == null)
                     || (this.upperBound != null && this.upperBound.equals(size.upperBound)));

        },

        hashCode: function() {
            var hashValue = this.basis.hashCode();
            if (this.lowerBound) {
                hashValue = hashValue * 37 + this.lowerBound.hashCode();
            }
            if (this.upperBound) {
                hashValue = hashValue * 37 + this.upperBound.hashCode();
            }
            return hashValue;
        }
    }
});

th.formlayout.ConstantSize = Class.define({
    members: {
        init: function(value, unit) {
            this.value = value;
            this.unit = unit;
        },

        getValue: function() {
            return this.value;
        },

        getUnit: function() {
            return this.unit;
        },

        getPixelSize: function(component) {
            if (this.unit == th.formlayout.ConstantSize.PIXEL)
                return this.intValue();
            else if (this.unit == th.formlayout.ConstantSize.EM)
                return th.formlayout.Sizes.emAsPixel(this.value, component);
            else if (this.unit == th.formlayout.ConstantSize.POINT)
                return th.formlayout.Sizes.pointAsPixel(this.intValue(), component);
            else if (this.unit == th.formlayout.ConstantSize.INCH)
                return th.formlayout.Sizes.inchAsPixel(this.value, component);
            else if (this.unit == th.formlayout.ConstantSize.MILLIMETER)
                return th.formlayout.Sizes.millimeterAsPixel(this.value, component);
            else if (this.unit == th.formlayout.ConstantSize.CENTIMETER)
                return th.formlayout.Sizes.centimeterAsPixel(this.value, component);
            else if (this.unit == th.formlayout.ConstantSize.DIALOG_UNITS_X)
                return th.formlayout.Sizes.dialogUnitXAsPixel(this.intValue(), component);
            else if (this.unit == th.formlayout.ConstantSize.DIALOG_UNITS_Y)
                return th.formlayout.Sizes.dialogUnitYAsPixel(this.intValue(), component);
            else
                throw "Invalid unit " + this.unit;
        },

        maximumSize: function(container, components, minMeasure, prefMeasure, defaultMeasure) {
            return this.getPixelSize(container);
        },

        compressible: function() {
            return false;
        },

        equals: function(o) {
            if (this == o)
                return true;
            if (!(o instanceof th.formlayout.ConstantSize))
                return false;
            var size = o;
            return this.value == size.value
                 && this.unit  == size.unit;
        },

        hashCode: function() {
            return this.value + 37 * this.unit.hashCode();
        },

        toString: function() {
            return this.value + unit.abbreviation();
        },

        encode: function() {
            return this.value + unit.encode();
        },

        intValue: function() {
            return Math.round(this.value);
        }
    }
});

th.formlayout.ConstantSize.valueOf = function(encodedValueAndUnit, horizontal) {
    var split = th.formlayout.ConstantSize.splitValueAndUnit(encodedValueAndUnit);
    var encodedValue = split[0];
    var encodedUnit  = split[1];
    var unit = th.formlayout.Unit.valueOf(encodedUnit, horizontal);
    var value = parseFloat(encodedValue);
    if (unit.requiresIntegers) {
        if (value != parseInt(value))
            throw unit.toString()
                + " value " + encodedValue + " must be an integer.";
    }
    return new th.formlayout.ConstantSize(value, unit);
};

th.formlayout.ConstantSize.dluX = function(value) {
    return new th.formlayout.ConstantSize(value, th.formlayout.ConstantSize.DLUX);
};

th.formlayout.ConstantSize.dluY = function(value) {
    return new th.formlayout.ConstantSize(value, th.formlayout.ConstantSize.DLUY);
};

th.formlayout.ConstantSize.splitValueAndUnit = function(encodedValueAndUnit) {
    var result = [];
    var len = encodedValueAndUnit.length;
    var firstLetterIndex = len;
    while (firstLetterIndex > 0
            && (! /[^a-zA-Z]/.test(encodedValueAndUnit.charAt(firstLetterIndex-1)))) {
            firstLetterIndex--;
    }
    result[0] = encodedValueAndUnit.substring(0, firstLetterIndex);
    result[1] = encodedValueAndUnit.substring(firstLetterIndex);
    return result;
};

th.formlayout.ConstantSize.PIXEL = new th.formlayout.Unit("Pixel", "px", undefined, true);
th.formlayout.ConstantSize.EM = new th.formlayout.Unit("Em", "em", undefined, false);
th.formlayout.ConstantSize.POINT = new th.formlayout.Unit("Point", "pt", undefined, false);
th.formlayout.ConstantSize.DIALOG_UNITS_X = new th.formlayout.Unit("Dialog units X", "dluX", "dlu", true);
th.formlayout.ConstantSize.DIALOG_UNITS_Y = new th.formlayout.Unit("Dialog units Y", "dluY", "dlu", true);
th.formlayout.ConstantSize.MILLIMETER = new th.formlayout.Unit("Millimeter", "mm", undefined, false);
th.formlayout.ConstantSize.CENTIMETER = new th.formlayout.Unit("Centimeter", "cm", undefined, false);
th.formlayout.ConstantSize.INCH = new th.formlayout.Unit("Inch", "in", undefined, false);

th.formlayout.ConstantSize.PX = th.formlayout.ConstantSize.PIXEL;
th.formlayout.ConstantSize.PT = th.formlayout.ConstantSize.POINT;
th.formlayout.ConstantSize.DLUX = th.formlayout.ConstantSize.DIALOG_UNITS_X;
th.formlayout.ConstantSize.DLUY = th.formlayout.ConstantSize.DIALOG_UNITS_Y;
th.formlayout.ConstantSize.MM = th.formlayout.ConstantSize.MILLIMETER;
th.formlayout.ConstantSize.CM = th.formlayout.ConstantSize.CENTIMETER;
th.formlayout.ConstantSize.IN = th.formlayout.ConstantSize.INCH;

th.formlayout.PrototypeSize = Class.define({
    members: {
        init: function(prototype) {
            this.proto = prototype;
        },

        getPrototype: function() {
            return this.proto;
        },

        maximumSize: function(container, components, minMeasure, prefMeasure, defaultMeasure) {
            throw "PrototypeSize.maximumSize() not yet implemented";

        },

        compressible: function() {
            return false;
        },

        encode: function() {
            return "'" + this.proto + "'";
        },

        equals: function(o) {
            if (this == o)
                return true;
            if (!(o instanceof th.formlayout.PrototypeSize))
                return false;
            return this.proto == o.proto;
        },

        toString: function() {
            return this.encode();
        }
    }
});

th.formlayout.ComponentSize = Class.define({
    members: {
        init: function(name) {
            this.name = name;
        },

        maximumSize: function(
            container,
            components,
            minMeasure,
            prefMeasure,
            defaultMeasure) {

            var measure = this == th.formlayout.Sizes.MINIMUM
                    ? minMeasure
                    : (this == th.formlayout.Sizes.PREFERRED ? prefMeasure : defaultMeasure);
            var maximum = 0;
            for (var i = 0; i < components.length; i++) {
                var c = components[i];
                maximum = Math.max(maximum, measure(c));
            }
            return maximum;
        },

        compressible: function() {
            return this == th.formlayout.Sizes.DEFAULT;
        },

        toString: function() {
            return this.encode();
        },

        encode: function() {
            return name.substring(0, 1);
        }
    }
});

th.formlayout.ComponentSize.valueOf = function(str) {
    if (str == "m" || str == "min")
        return th.formlayout.Sizes.MINIMUM;
    if (str == "p" || str == "pref")
        return th.formlayout.Sizes.PREFERRED;
    if (str == "d" || str == "default")
        return th.formlayout.Sizes.DEFAULT;
    return;
}

th.formlayout.DefaultUnitConverter = Class.define({
    members: {
        inchAsPixel: function(inch, component) {
            return th.convertAbsoluteUnitToPixels("in", inch);
        },

        millimeterAsPixel: function(mm, component) {
            return th.convertAbsoluteUnitToPixels("mm", mm);
        },

        centimeterAsPixel: function(cm, component) {
            return th.convertAbsoluteUnitToPixels("cm", cm);
        },

        pointAsPixel: function(pt, component) {
            return th.convertAbsoluteUnitToPixels("pt", pt);
        },

        emAsPixel: function(em, component) {
            return th.convertEmToPixels(component.cssValue("font"), em);
        },

        dialogUnitXAsPixel: function(dluX, component) {
            throw "Unsupported";
        },

        dialogUnitYAsPixel: function(dluY, component) {
            throw "Unsupported";
        }
    }
});

th.formlayout.Sizes = {};

th.formlayout.Sizes.unitConverter = new th.formlayout.DefaultUnitConverter();

th.formlayout.Sizes.pixel = function(value) {
    return new th.formlayout.ConstantSize(value, th.formlayout.ConstantSize.PIXEL);
}

th.formlayout.Sizes.ZERO = th.formlayout.Sizes.pixel(0);

th.formlayout.Sizes.DLUX1 = th.formlayout.ConstantSize.dluX(1);
th.formlayout.Sizes.DLUX2 = th.formlayout.ConstantSize.dluX(2);
th.formlayout.Sizes.DLUX3 = th.formlayout.ConstantSize.dluX(3);
th.formlayout.Sizes.DLUX4 = th.formlayout.ConstantSize.dluX(4);
th.formlayout.Sizes.DLUX5 = th.formlayout.ConstantSize.dluX(5);
th.formlayout.Sizes.DLUX6 = th.formlayout.ConstantSize.dluX(6);
th.formlayout.Sizes.DLUX7 = th.formlayout.ConstantSize.dluX(7);
th.formlayout.Sizes.DLUX8 = th.formlayout.ConstantSize.dluX(8);
th.formlayout.Sizes.DLUX9 = th.formlayout.ConstantSize.dluX(9);
th.formlayout.Sizes.DLUX11 = th.formlayout.ConstantSize.dluX(11);
th.formlayout.Sizes.DLUX14 = th.formlayout.ConstantSize.dluX(14);
th.formlayout.Sizes.DLUX21 = th.formlayout.ConstantSize.dluX(21);

th.formlayout.Sizes.DLUY1 = th.formlayout.ConstantSize.dluY(1);
th.formlayout.Sizes.DLUY2 = th.formlayout.ConstantSize.dluY(2);
th.formlayout.Sizes.DLUY3 = th.formlayout.ConstantSize.dluY(3);
th.formlayout.Sizes.DLUY4 = th.formlayout.ConstantSize.dluY(4);
th.formlayout.Sizes.DLUY5 = th.formlayout.ConstantSize.dluY(5);
th.formlayout.Sizes.DLUY6 = th.formlayout.ConstantSize.dluY(6);
th.formlayout.Sizes.DLUY7 = th.formlayout.ConstantSize.dluY(7);
th.formlayout.Sizes.DLUY8 = th.formlayout.ConstantSize.dluY(8);
th.formlayout.Sizes.DLUY9 = th.formlayout.ConstantSize.dluY(9);
th.formlayout.Sizes.DLUY11 = th.formlayout.ConstantSize.dluY(11);
th.formlayout.Sizes.DLUY14 = th.formlayout.ConstantSize.dluY(14);
th.formlayout.Sizes.DLUY21 = th.formlayout.ConstantSize.dluY(21);

th.formlayout.Sizes.MINIMUM = new th.formlayout.ComponentSize("minimum");
th.formlayout.Sizes.PREFERRED = new th.formlayout.ComponentSize("preferred");
th.formlayout.Sizes.DEFAULT = new th.formlayout.ComponentSize("default");
th.formlayout.Sizes.defaultUnit = th.formlayout.ConstantSize.PIXEL;

th.formlayout.Sizes.constant = function(encodedValueAndUnit, horizontal) {
    var lowerCase = encodedValueAndUnit.toLowerCase();
    var trimmed = th.trim(lowerCase);
    return th.formlayout.ConstantSize.valueOf(trimmed, horizontal);
}

th.formlayout.Sizes.bounded = function(basis, lowerBound, upperBound) {
    return th.formlayout.BoundedSize(basis, lowerBound, upperBound);
}

th.formlayout.Sizes.emAsPixel = function(em, component) {
    return em == 0 ? 0 : th.formlayout.Sizes.unitConverter.emAsPixel(em, component);
}

th.formlayout.Sizes.inchAsPixel = function(inch, component) {
    return inch == 0 ? 0 : th.formlayout.Sizes.unitConverter.inchAsPixel(inch, component);
}

th.formlayout.Sizes.millimeterAsPixel = function(mm, component) {
    return mm == 0 ? 0 : th.formlayout.Sizes.unitConverter.millimeterAsPixel(mm, component);
}

th.formlayout.Sizes.centimeterAsPixel = function(cm, component) {
    return cm == 0 ? 0 : th.formlayout.Sizes.unitConverter.centimeterAsPixel(cm, component);
}

th.formlayout.Sizes.pointAsPixel = function(pt, component) {
    return pt == 0 ? 0 : th.formlayout.Sizes.unitConverter.pointAsPixel(pt, component);
}

th.formlayout.Sizes.dialogUnitXAsPixel = function(dluX, component) {
    return dluX == 0 ? 0 : th.formlayout.Sizes.unitConverter.dialogUnitXAsPixel(dluX, component);
}

th.formlayout.Sizes.dialogUnitYAsPixel = function(dluY, component) {
    return dluY == 0 ? 0 : th.formlayout.Sizes.unitConverter.dialogUnitYAsPixel(dluY, component);
}

th.formlayout.Sizes.getDefaultUnit = function() {
    return th.formlayout.Sizes.defaultUnit;
}

th.formlayout.Alignment = Class.define({
    members: {
        init: function(name, orientation) {
            this.name = name;
            this.orientation = orientation;
        },

        toString: function() {
            return this.name;
        },

        abbreviation: function() {
            return this.name.charAt(0);
        },

        isHorizontal: function() {
            return this.orientation != th.formlayout.Alignment.VERTICAL;
        },

        isVertical: function() {
            return this.orientation != th.formlayout.Alignment.HORIZONTAL;
        }
    }
});

th.formlayout.Alignment.HORIZONTAL = 0;
th.formlayout.Alignment.VERTICAL = 1;
th.formlayout.Alignment.BOTH = 2;

th.formlayout.Alignment.valueOf = function(nameOrAbbreviation) {
    var str = nameOrAbbreviation.toLowerCase();
    if (str == "d" || str == "default")
        return th.formlayout.CellConstraints.DEFAULT;
    else if (str == "f" || str == "fill")
        return th.formlayout.CellConstraints.FILL;
    else if (str == "c" || str == "center")
        return th.formlayout.CellConstraints.CENTER;
    else if (str == "l" || str == "left")
        return th.formlayout.CellConstraints.LEFT;
    else if (str == "r" || str == "right")
        return th.formlayout.CellConstraints.RIGHT;
    else if (str == "t" || str == "top")
        return th.formlayout.CellConstraints.TOP;
    else if (str == "b" || str == "bottom")
        return th.formlayout.CellConstraints.BOTTOM;
    else
        throw "Invalid alignment " + nameOrAbbreviation
            + ". Must be one of: left, center, right, top, bottom, "
            + "fill, default, l, c, r, t, b, f, d.";

}

th.formlayout.CellConstraints = Class.define({
    members: {
        init: function(gridX, gridY, gridWidth, gridHeight, hAlign, vAlign, insets) {
            var encodedConstraints = undefined;
            if (th.isString(gridX)) {
                encodedConstraints = gridX;
                delete gridX;
            }

            this.gridX = (gridX == undefined) ? 1 : gridX;
            this.gridY = (gridY == undefined) ? 1 : gridY;
            this.gridWidth = (gridWidth == undefined) ? 1 : gridWidth;
            this.gridHeight = (gridHeight == undefined) ? 1 : gridHeight;
            this.hAlign = hAlign || th.formlayout.CellConstraints.DEFAULT;
            this.vAlign = vAlign || th.formlayout.CellConstraints.DEFAULT;
            this.insets = insets || th.formlayout.CellConstraints.EMPTY_INSETS;

            if (this.gridX <= 0)
                throw new th.formlayout.IndexOutOfBoundsException("The grid x must be a positive number.");
            if (this.gridY <= 0)
                throw new th.formlayout.IndexOutOfBoundsException("The grid y must be a positive number.");
            if (this.gridWidth <= 0)
                throw new th.formlayout.IndexOutOfBoundsException("The grid width must be a positive number.");
            if (this.gridHeight <= 0)
                throw new th.formlayout.IndexOutOfBoundsException("The grid height must be a positive number.");
            this.ensureValidOrientations(this.hAlign, this.vAlign);

            if (encodedConstraints) {
                this.initFromConstraints(encodedConstraints);
            }
        },

        ensureValidOrientations: function(horizontalAlignment, verticalAlignment) {
            if (!horizontalAlignment.isHorizontal())
                throw new th.formlayout.IllegalArgumentException("The horizontal alignment must be one of: left, center, right, fill, default.");
            if (!verticalAlignment.isVertical())
                throw new th.formlayout.IllegalArgumentException("The vertical alignment must be one of: top, center, botto, fill, default.");
        },

        initFromConstraints: function(encodedConstraints) {
            var tokens = encodedConstraints.split(",");
            var argCount = tokens.length;
            if (!(argCount == 2 || argCount == 4 || argCount == 6))
               throw new th.formlayout.IllegalArgumentException(
                        "You must provide 2, 4 or 6 arguments.");

            var currentToken = 0;
            var nextInt = parseInt(th.trim(tokens[currentToken++]));
            if (!nextInt) {
                throw new th.formlayout.IllegalArgumentException(
                        "First cell constraint element must be a number.");
            }
            this.gridX = nextInt;
            if (this.gridX <= 0)
                throw new th.formlayout.IndexOutOfBoundsException("The grid x must be a positive number.");

            nextInt = parseInt(th.trim(tokens[currentToken++]));
            if (!nextInt) {
                throw new th.formlayout.IllegalArgumentException(
                        "Second cell constraint element must be a number.");
            }
            this.gridY = nextInt;
            if (this.gridY <= 0)
                throw new th.formlayout.IndexOutOfBoundsException(
                        "The grid y must be a positive number.");

            if (currentToken >= tokens.length)
                return;

            var token = th.trim(tokens[currentToken++]);
            nextInt = parseInt(token);
            if (nextInt) {
                this.gridWidth = nextInt;
                if (this.gridWidth <= 0)
                    throw new th.formlayout.IndexOutOfBoundsException(
                        "The grid width must be a positive number.");
                nextInt = parseInt(th.trim(tokens[currentToken++]));
                if (nextInt == null)
                    throw new th.formlayout.IllegalArgumentException(
                        "Fourth cell constraint element must be like third.");
                this.gridHeight = nextInt;
                if (this.gridHeight <= 0)
                    throw new th.formlayout.IndexOutOfBoundsException(
                        "The grid height must be a positive number.");

                if (currentToken >= tokens.length)
                    return;
                token = th.trim(tokens[currentToken++]);
            }

            this.hAlign = this.decodeAlignment(token);
            this.vAlign = this.decodeAlignment(th.trim(tokens[currentToken++]));
            this.ensureValidOrientations(this.hAlign, this.vAlign);
        },

        ensureValidGridBounds: function(colCount, rowCount) {
            if (this.gridX <= 0) {
                throw new th.formlayout.IndexOutOfBoundsException(
                    "The column index " + this.gridX + " must be positive.");
            }
            if (this.gridX > colCount) {
                throw new th.formlayout.IndexOutOfBoundsException(
                    "The column index " + this.gridX + " must be less than or equal to "
                        + colCount + ".");
            }
            if (this.gridX + this.gridWidth - 1 > colCount) {
                throw new th.formlayout.IndexOutOfBoundsException(
                    "The grid width " + this.gridWidth + " must be less than or equal to "
                        + (colCount - this.gridX + 1) + ".");
            }
            if (this.gridY <= 0) {
                throw new th.formlayout.IndexOutOfBoundsException(
                    "The row index " + this.gridY + " must be positive.");
            }
            if (this.gridY > rowCount) {
                throw new th.formlayout.IndexOutOfBoundsException(
                    "The row index " + this.gridY + " must be less than or equal to "
                        + rowCount + ".");
            }
            if (this.gridY + this.gridHeight - 1 > rowCount) {
                throw new th.formlayout.IndexOutOfBoundsException(
                    "The grid height " + this.gridHeight + " must be less than or equal to "
                        + (rowCount - this.gridY + 1) + ".");
            }
        },

        decodeAlignment: function(encodedAlignment) {
            return th.formlayout.Alignment.valueOf(encodedAlignment);
        },

        setBounds: function(c, layout,
                       cellBounds,
                       minWidthMeasure,
                       minHeightMeasure,
                       prefWidthMeasure,
                       prefHeightMeasure) {
            var colSpec = this.gridWidth  == 1 ? layout.getColumnSpec(this.gridX) : undefined;
            var rowSpec = this.gridHeight == 1 ? layout.getRowSpec(this.gridY) : undefined;
            var concreteHAlign = this.concreteAlignment(this.hAlign, colSpec);
            var concreteVAlign = this.concreteAlignment(this.vAlign, rowSpec);
            var concreteInsets = this.insets ? this.insets : th.formlayout.CellConstraints.EMPTY_INSETS;
            var cellX = cellBounds.x + concreteInsets.left;
            var cellY = cellBounds.y + concreteInsets.top;
            var cellW = cellBounds.width  - concreteInsets.left - concreteInsets.right;
            var cellH = cellBounds.height - concreteInsets.top  - concreteInsets.bottom;
            var compW = this.componentSize(c, colSpec, cellW, minWidthMeasure,
                                                         prefWidthMeasure);
            var compH = this.componentSize(c, rowSpec, cellH, minHeightMeasure,
                                                         prefHeightMeasure);
            var x = this.origin(concreteHAlign, cellX, cellW, compW);
            var y = this.origin(concreteVAlign, cellY, cellH, compH);
            var w = this.extent(concreteHAlign, cellW, compW);
            var h = this.extent(concreteVAlign, cellH, compH);
            c.setBounds(x, y, w, h);
        },

        concreteAlignment: function(cellAlignment, formSpec) {
            return !formSpec
                ? (cellAlignment == th.formlayout.CellConstraints.DEFAULT ? th.formlayout.CellConstraints.FILL : cellAlignment)
                : this.usedAlignment(cellAlignment, formSpec);
        },

        usedAlignment: function(cellAlignment, formSpec) {
            if (cellAlignment != th.formlayout.CellConstraints.DEFAULT) {
                return cellAlignment;
            }
            var defaultAlignment = formSpec.getDefaultAlignment();
            if (defaultAlignment == th.formlayout.FormSpec.FILL_ALIGN)
                return th.formlayout.CellConstraints.FILL;
            if (defaultAlignment == th.formlayout.ColumnSpec.LEFT)
                return th.formlayout.CellConstraints.LEFT;
            else if (defaultAlignment == th.formlayout.FormSpec.CENTER_ALIGN)
                return th.formlayout.CellConstraints.CENTER;
            else if (defaultAlignment == th.formlayout.ColumnSpec.RIGHT)
                return th.formlayout.CellConstraints.RIGHT;
            else if (defaultAlignment == th.formlayout.RowSpec.TOP)
                return th.formlayout.CellConstraints.TOP;
            else
                return th.formlayout.CellConstraints.BOTTOM;
        },

        componentSize: function(component,
                                   formSpec,
                                   cellSize,
                                   minMeasure,
                                   prefMeasure) {
            if (!formSpec) {
                return prefMeasure(component);
            } else if (formSpec.getSize() == th.formlayout.Sizes.MINIMUM) {
                return minMeasure(component);
            } else if (formSpec.getSize() == th.formlayout.Sizes.PREFERRED) {
                return prefMeasure(component);
            } else {  // default mode
                return Math.min(cellSize, prefMeasure(component));
            }
        },

        origin: function(alignment, cellOrigin, cellSize, componentSize) {
            if (alignment == th.formlayout.CellConstraints.RIGHT || alignment == th.formlayout.CellConstraints.BOTTOM) {
                return cellOrigin + cellSize - componentSize;
            } else if (alignment == th.formlayout.CellConstraints.CENTER) {
                return cellOrigin + (cellSize - componentSize) / 2;
            } else {  // left, top, fill
                return cellOrigin;
            }
        },

        extent: function(alignment, cellSize, componentSize) {
            return alignment == th.formlayout.CellConstraints.FILL
                        ? cellSize
                        : componentSize;
        },

        toString: function() {
            var buffer = "CellConstraints";
            buffer += "[x=";
            buffer += this.gridX;
            buffer += "; y=";
            buffer += this.gridY;
            buffer += "; w=";
            buffer += this.gridWidth;
            buffer += "; h=";
            buffer += this.gridHeight;
            buffer += "; hAlign=";
            buffer += this.hAlign;
            buffer += "; vAlign=";
            buffer += this.vAlign;
            buffer += "; honorsVisibility=";
            buffer += this.honorsVisibility;

            buffer += ']';
            return buffer;
        },

        xy: function () {
            var args = arguments;
            switch (arguments.length) {
                case 2:
                    return this.xywh(args[0], args[1], 1, 1);
                case 3:
                    return this.xywh(args[0], args[1], 1, 1, args[2]);
                case 4:
                    return this.xywh(args[0], args[1], 1, 1, args[2], args[3]);
                default:
                    throw "wrong number of arguments passed to xy()";
            }
        },

        xyw: function () {
            var args = arguments;
            switch (arguments.length) {
                case 3:
                    return this.xywh(args[0], args[1], args[2], 1, th.formlayout.CellConstraints.DEFAULT, th.formlayout.CellConstraints.DEFAULT);
                case 4:
                    return this.xywh(args[0], args[1], args[2], 1, args[3]);
                case 5:
                    return this.xywh(args[0], args[1], args[2], 1, args[3], args[4]);
                default:
                    throw "wrong number of arguments passed to xyw()";
            }
        },

        xywh: function () {
            var args = arguments;
            switch (arguments.length) {
                case 4:
                    return this.xywh(args[0], args[1], args[2], args[3], th.formlayout.CellConstraints.DEFAULT, th.formlayout.CellConstraints.DEFAULT);
                case 5:
                    var result = this.xywh(args[0], args[1], args[2], args[3]);
                    result.setAlignment(args[4], true);
                    return result;
                case 6:
                    this.gridX = args[0];
                    this.gridY = args[1];
                    this.gridWidth = args[2];
                    this.gridHeight = args[3];
                    this.hAlign = args[4];
                    this.vAlign = args[5];
                    this.ensureValidOrientations(this.hAlign, this.vAlign);
                    return this;
                default:
                    throw "wrong number of arguments passed to xywh()";
            }
        }
    }
});

th.formlayout.CellConstraints.DEFAULT = new th.formlayout.Alignment("default", th.formlayout.Alignment.BOTH);
th.formlayout.CellConstraints.FILL = new th.formlayout.Alignment("fill", th.formlayout.Alignment.BOTH);
th.formlayout.CellConstraints.LEFT = new th.formlayout.Alignment("left", th.formlayout.Alignment.HORIZONTAL);
th.formlayout.CellConstraints.RIGHT = new th.formlayout.Alignment("right", th.formlayout.Alignment.HORIZONTAL);
th.formlayout.CellConstraints.CENTER = new th.formlayout.Alignment("center", th.formlayout.Alignment.BOTH);
th.formlayout.CellConstraints.TOP = new th.formlayout.Alignment("top", th.formlayout.Alignment.VERTICAL);
th.formlayout.CellConstraints.BOTTOM = new th.formlayout.Alignment("bottom", th.formlayout.Alignment.VERTICAL);

th.formlayout.CellConstraints.EMPTY_INSETS = { top: 0, left: 0, bottom: 0, right: 0 };

th.formlayout.FormSpecParser = Class.define({
    members: {
        init: function(source, description, layoutMap, horizontal) {
            this.layoutMap = layoutMap;
            this.source = source;   // fixme: variables not yet supported
        },

        parseColumnSpecs: function() {
            var encodedColumnSpecs = this.split(this.source, 0);
            var columnCount = encodedColumnSpecs.length;
            var columnSpecs = [];
            for (var i = 0; i < columnCount; i++) {
                var encodedSpec = encodedColumnSpecs[i];
                columnSpecs[i] = th.formlayout.ColumnSpec.decodeExpanded(encodedSpec);
            }
            return columnSpecs;
        },

        parseRowSpecs: function() {
            var encodedRowSpecs = this.split(this.source, 0);
            var rowCount = encodedRowSpecs.length;
            var rowSpecs = [];
            for (var i = 0; i < rowCount; i++) {
                var encodedSpec = encodedRowSpecs[i];
                rowSpecs[i] = th.formlayout.RowSpec.decodeExpanded(encodedSpec);
            }
            return rowSpecs;
        },

        split: function(expression, offset) {
            var encodedSpecs = [];
            var parenthesisLevel = 0;  // number of open '('
            var bracketLevel = 0;      // number of open '['
            var length = expression.length;
            var specStart = 0;
            var c;
            var lead = true;
            for (var i = 0; i < length; i++) {
                c = expression.charAt(i);
                if (lead && th.isWhitespace(c)) {
                    specStart++;
                    continue;
                }
                lead = false;
                if ((c == ',') && (parenthesisLevel == 0) && (bracketLevel == 0)) {
                    var token = expression.substring(specStart, i);
                    this.addSpec(encodedSpecs, token, offset + specStart);
                    specStart = i + 1;
                    lead = true;
                } else if (c == '(') {
                    if (bracketLevel > 0) {
                        this.fail(offset + i, "illegal '(' in [...]");
                    }
                    parenthesisLevel++;
                } else if (c == ')') {
                    if (bracketLevel > 0) {
                        this.fail(offset + i, "illegal ')' in [...]");
                    }
                    parenthesisLevel--;
                    if (parenthesisLevel < 0) {
                        this.fail(offset + i, "missing '('");
                    }
                } else if (c == '[') {
                    if (bracketLevel > 0) {
                        this.fail(offset + i, "too many '['");
                    }
                    bracketLevel++;
                } else if (c == ']') {
                    bracketLevel--;
                    if (bracketLevel < 0) {
                        this.fail(offset + i, "missing '['");
                    }
                }
            }
            if (parenthesisLevel > 0) {
                this.fail(offset + length, "missing ')'");
            }
            if (bracketLevel > 0) {
                this.fail(offset + length, "missing ']");
            }
            if (specStart < length) {
                var token = expression.substring(specStart);
                this.addSpec(encodedSpecs, token, offset + specStart);
            }
            return encodedSpecs;

        },

        addSpec: function(encodedSpecs, expression, offset) {
            var trimmedExpression = th.trim(expression);
            var multiplier = this.multiplier(trimmedExpression, offset);
            if (!multiplier) {
                encodedSpecs.push(trimmedExpression);
                return;
            }
            var subTokenList = this.split(multiplier.expression, offset + multiplier.offset);
            for (var i = 0; i < multiplier.multiplier; i++) {
                th.forEach(subTokenList, function(token) {
                    encodedSpecs.push(token);
                });
            }
        },

        multiplier: function(expression, offset) {
            var matcher = new RegExp(th.formlayout.FormSpecParser.MULTIPLIER_PREFIX_PATTERN);
            if (!matcher.test(expression)) {
                return;
            }

            var matcherResults = matcher.exec(expression);
            matcherResults.end = matcherResults.index + matcherResults[0].length;
            if (matcherResults.index > 0) {
                this.fail(offset + matcherResults.index, "illegal multiplier position");
            }


            var digitMatcher = new RegExp(th.formlayout.DIGIT_PATTERN);
            if (!digitMatcher.test(expression)) {
                return;
            }

            var digitResults = digitMatcher.exec(expression);
            digitResults.end = matcherResults.index + matcherResults[0].length;
            var digitStr = expression.substring(0, digitResults.end);
            var number = parseInt(digitStr);
            if (isNaN(number)) {
                this.fail(offset, "Invalid multiplier");
            }
            if (number <= 0) {
                this.fail(offset, "illegal 0 multiplier");
            }
            var subexpression = expression.substring(matcherResults.end, expression.length() - 1);
            return { multiplier: number, expression: subexpression, offset: matcherResults.end };
        },

        fail: function(index, description, source) {
            if (!source) source = this.source;
            throw new th.formlayout.FormLayoutParseException(this.message(source, index, description));
        },

        message: function(source, index, description) {
            var buffer = "\n";
            buffer += '\n';
            buffer += source;
            buffer += '\n';
            for (var i = 0; i < index; i++) {
                buffer += ' ';
            }
            buffer += '^';
            buffer += description;
            return buffer;
        }
    }
});

th.formlayout.FormSpecParser.parseColumnSpecs = function(encodedColumnSpecs, layoutMap) {
    var parser = new th.formlayout.FormSpecParser(
            encodedColumnSpecs,
            "encoded column specifications",
            layoutMap,
            true);
    return parser.parseColumnSpecs();
}

th.formlayout.FormSpecParser.parseRowSpecs = function(encodedRowSpecs, layoutMap) {
    var parser = new th.formlayout.FormSpecParser(
            encodedRowSpecs,
            "encoded column specifications",
            layoutMap,
            false);
    return parser.parseRowSpecs();
}

th.formlayout.FormSpecParser.MULTIPLIER_PREFIX_PATTERN = "\\d+\\s*\\*\\s*\\(";
th.formlayout.FormSpecParser.DIGIT_PATTERN = "\\d+";
