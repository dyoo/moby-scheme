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

// = Simple Syntax Highlighting =
//
// Not prepared for running in a worker thread.
// Woul be more overhead than benefit for auch a simple highlighter

dojo.provide("bespin.syntax.simple._base");


// ** {{{ bespin.syntax.simple.Model }}} **
//
// Tracks syntax highlighting data on a per-line basis.
dojo.declare("bespin.syntax.simple.Model", bespin.syntax.Model, {
    lineMetaInfo:  [],
    // ** {{{ Meta Info }}} **
    //
    // We store meta info on the lines, such as the fact that it is in a multiline comment
    setLineMetaInfo: function(lineNumber, meta) {
        this.lineMetaInfo[lineNumber] = meta;
    },

    getLineMetaInfo: function(lineNumber) {
        return this.lineMetaInfo[lineNumber];
    },

    getSyntaxStylesPerLine: function(lineText, lineNumber, language) {
        if (this.language != language) {
            this.engine = bespin.syntax.simple.Resolver.resolve(language);
            this.language = language;
        }

        // Get the row contents as one string
        var syntaxResult = { // setup the result
            text: lineText,
            regions: []
        };

        var meta;

        // we have the ability to have subtypes within the main parser
        // E.g. HTML can have JavaScript or CSS within
        if (typeof this.engine['innertypes'] == "function") {
            var languages = this.engine.innertypes(lineText);

            for (var i = 0; i < languages.length; i++) {
                var type = languages[i];
                meta = { inMultiLineComment: this.inMultiLineComment(), offset: type.start }; // pass in an offset
                var pieceRegions = [];
                var fromResolver = bespin.syntax.simple.Resolver.highlight(type.type, lineText.substring(type.start, type.stop), meta);
                if (fromResolver.meta && (i == languages.length - 1) ) {
                    this.setLineMetaInfo(lineNumber, fromResolver.meta);
                }
                pieceRegions.push(fromResolver);
            }
            syntaxResult.regions.push(this.mergeSyntaxResults(pieceRegions));
        } else {
            meta = (lineNumber > 0) ? this.getLineMetaInfo(lineNumber - 1) : {};
            var result = this.engine.highlight(lineText, meta);
            this.setLineMetaInfo(lineNumber, result.meta);
            syntaxResult.regions.push(result.regions);
        }

        return syntaxResult;
    }
});


// ** {{{ bespin.syntax.simple.Resolver }}} **
//
// The resolver holds the engines per language that are available to do the actual syntax highlighting
bespin.syntax.simple.Resolver = new function() {
  var engines = {};

  // ** {{{ NoopSyntaxEngine }}} **
  //
  // Return a plain region that is the entire line
  var NoopSyntaxEngine = {
      highlight: function(line, meta) {
          return { regions: {
              plain: [{
                  start: 0,
                  stop: line.length
              }]
          } };
      }
      //innersyntax: function() {},
  };

  return {
      // ** {{{ highlight }}} **
      //
      // A high level highlight function that uses the {{{type}}} to get the engine, and asks it to highlight
      highlight: function(type, line, meta, lineNumber) {
          this.resolve(type).highlight(line, meta, lineNumber);
      },

      // ** {{{ register }}} **
      //
      // Engines register themselves,
      // e.g. {{{bespin.syntax.EngineResolver.register(new bespin.syntax.CSSSyntaxEngine(), ['css']);}}}
      register: function(syntaxEngine, types) {
          for (var i = 0; i < types.length; i++) {
              engines[types[i]] = syntaxEngine;
          }
      },

      // ** {{{ resolve }}} **
      //
      // Hunt down the engine for the given {{{type}}} (e.g. css, js, html) or return the {{{NoopSyntaxEngine}}}
      resolve: function(type) {
          return engines[type] || NoopSyntaxEngine;
      }
  };
}();

