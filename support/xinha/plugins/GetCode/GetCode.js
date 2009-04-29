/**
 Implemented now as GetHtmlImplementation plugin in modules/GetHtml/TransformInnerHTML.js
  */

function GetCode(editor) {
    this.editor = editor;
    var cfg = editor.config;
    var self = this;
}


GetCode._pluginInfo = {
    name          : "GetCode",
    version       : "1.0",
    developer     : "Danny Yoo",
    developer_url : "http://hashcollision.org/",
    sponsor       : "",
    sponsor_url   : "",
    license       : "htmlArea"
};




(function() {

    // Retrieves the HTML code from the given node.	 This is a replacement for
    // getting innerHTML, using standard DOM calls.
    // Wrapper legacy see #442
    GetCode.prototype.getCode = function(editor)
    {
	alert(editor._doc.body.innerHTML);
	return _getCode(editor._doc.body, false, editor).join("");
    };


    GetCode.prototype.toHtml = function(s) {
	return htmlentities(s).replace('\n', "<br/>").replace(" ", "&nbsp;");
    };



    var emptyAttributes = " checked disabled ismap readonly nowrap compact declare selected defer multiple noresize noshade "



    function _getCode(root, outputRoot, editor, indent)
    {


	var html = [];
	if ( !indent )
	{
	    indent = '';
	}

	switch ( root.nodeType )
	{
	case 10:// Node.DOCUMENT_TYPE_NODE
	case 6: // Node.ENTITY_NODE
	case 12:// Node.NOTATION_NODE
	    // this all are for the document type, probably not necessary
	    break;

	case 2: // Node.ATTRIBUTE_NODE
	    // Never get here, this has to be handled in the ELEMENT case because
	    // of IE crapness requring that some attributes are grabbed directly from
	    // the attribute (nodeValue doesn't return correct values), see
	    //http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&oe=UTF-8&safe=off&selm=3porgu4mc4ofcoa1uqkf7u8kvv064kjjb4%404ax.com
	    // for information
	    break;

	case 4: // Node.CDATA_SECTION_NODE
	    // Mozilla seems to convert CDATA into a comment when going into wysiwyg mode,
	    //  don't know about IE
	    html.push((Xinha.is_ie ? ('\n' + indent) : '') + '<![CDATA[' + root.data + ']]>');
	    break;

	case 5: // Node.ENTITY_REFERENCE_NODE
	    html.push(html_entity_decode('&' + root.nodeValue + ';'));
	    break;

	case 7: // Node.PROCESSING_INSTRUCTION_NODE
	    // PI's don't seem to survive going into the wysiwyg mode, (at least in moz)
	    // so this is purely academic
	    html.push((Xinha.is_ie ? ('\n' + indent) : '') + '<'+'?' + root.target + ' ' + root.data + ' ?>');
	    break;

	case 1: // Node.ELEMENT_NODE
	case 11: // Node.DOCUMENT_FRAGMENT_NODE
	case 9: // Node.DOCUMENT_NODE
	    var closed;
	    var i;
	    var root_tag = (root.nodeType == 1) ? root.tagName.toLowerCase() : '';
	    if ( ( root_tag == "script" || root_tag == "noscript" ) && editor.config.stripScripts )
	    {
		break;
	    }
	    if ( outputRoot )
	    {
		outputRoot = !(editor.config.htmlRemoveTags && editor.config.htmlRemoveTags.test(root_tag));
	    }
	    if ( Xinha.is_ie && root_tag == "head" )
	    {
		if ( outputRoot )
		{
		    html.push((Xinha.is_ie ? ('\n' + indent) : '') + "<head>");
		}
		
		var save_multiline = RegExp.multiline;
		RegExp.multiline = true;
		var txt = 
		    root.innerHTML
		    .replace(Xinha.RE_tagName, function(str, p1, p2) { return p1 + p2.toLowerCase(); }) // lowercasize
		    .replace(/\s*=\s*(([^'"][^>\s]*)([>\s])|"([^"]+)"|'([^']+)')/g, '="$2$4$5"$3') //add attribute quotes
					.replace(/<(link|meta)((\s*\S*="[^"]*")*)>([\n\r]*)/g, '<$1$2 />\n'); //terminate singlet tags
        RegExp.multiline = save_multiline;
        html.push(txt + '\n');
        if ( outputRoot )
        {
          html.push((Xinha.is_ie ? ('\n' + indent) : '') + "</head>");
        }
        break;
      }
      else if ( outputRoot )
      {
        closed = (!(root.hasChildNodes() || Xinha.needsClosingTag(root)));
        html.push(((Xinha.isBlockElement(root)) ? ('\n' + indent) : ''));
        // html.push(((Xinha.isBlockElement(root)) ? ('\n' + indent) : '') + "<" + root.tagName.toLowerCase());
        var attrs = root.attributes;
        
        for ( i = 0; i < attrs.length; ++i )
        {
          var a = attrs.item(i);
          // In certain browsers (*cough* firefox) the dom node loses
          // information if the image is currently broken.  In order to prevent
          // corrupting the height and width of image tags, we strip height and
          // width from the image rather than reporting bad information.
          if (Xinha.is_real_gecko && (root.tagName.toLowerCase() == 'img') &&
              ((a.nodeName.toLowerCase() == 'height') || (a.nodeName.toLowerCase() == 'width')))
          {
            if (!root.complete || root.naturalWidth === 0)
            {
              // This means that firefox has been unable to read the dimensions from the actual image
              continue;
            }
          }
          if (typeof a.nodeValue == 'object' ) continue; // see #684
          if (root.tagName.toLowerCase() == "input" 
              && root.type.toLowerCase() == "checkbox" 
              && a.nodeName.toLowerCase() == "value"
              && a.nodeValue.toLowerCase() == "on") 
          {
            continue;
          }
          if ( !a.specified 
            // IE claims these are !a.specified even though they are.  Perhaps others too?
            && !(root.tagName.toLowerCase().match(/input|option/) && a.nodeName == 'value')
            && !(root.tagName.toLowerCase().match(/area/) && a.nodeName.match(/shape|coords/i)) 
          )
          {
            continue;
          }
          var name = a.nodeName.toLowerCase();
          if ( /_moz_editor_bogus_node/.test(name) || ( name == 'class' && a.nodeValue == 'webkit-block-placeholder') )
          {
            html = [];
            break;
          }
          if ( /(_moz)|(contenteditable)|(_msh)/.test(name) )
          {
            // avoid certain attributes
            continue;
          }
          var value;
          if ( emptyAttributes.indexOf(" "+name+" ") != -1)
          {
            value = name;
          }
          else if ( name != "style" )
          {
            // IE5.5 reports 25 when cellSpacing is
            // 1; other values might be doomed too.
            // For this reason we extract the
            // values directly from the root node.
            // I'm starting to HATE JavaScript
            // development.  Browser differences
            // suck.
            //
            // Using Gecko the values of href and src are converted to absolute links
            // unless we get them using nodeValue()
            if ( typeof root[a.nodeName] != "undefined" && name != "href" && name != "src" && !(/^on/.test(name)) )
            {
              value = root[a.nodeName];
            }
            else
            {
              value = a.nodeValue;
			  if (name == 'class')
			  {
			  	value = value.replace(/Apple-style-span/,'');
				if (!value) continue;
			  }
              // IE seems not willing to return the original values - it converts to absolute
              // links using a.nodeValue, a.value, a.stringValue, root.getAttribute("href")
              // So we have to strip the baseurl manually :-/
              if ( Xinha.is_ie && (name == "href" || name == "src") )
              {
                value = editor.stripBaseURL(value);
              }

              // High-ascii (8bit) characters in links seem to cause problems for some sites,
              // while this seems to be consistent with RFC 3986 Section 2.4
              // because these are not "reserved" characters, it does seem to
              // cause links to international resources not to work.  See ticket:167

              // IE always returns high-ascii characters un-encoded in links even if they
              // were supplied as % codes (it unescapes them when we pul the value from the link).

              // Hmmm, very strange if we use encodeURI here, or encodeURIComponent in place
              // of escape below, then the encoding is wrong.  I mean, completely.
              // Nothing like it should be at all.  Using escape seems to work though.
              // It's in both browsers too, so either I'm doing something wrong, or
              // something else is going on?

              if ( editor.config.only7BitPrintablesInURLs && ( name == "href" || name == "src" ) )
              {
                value = value.replace(/([^!-~]+)/g, function(match) { return escape(match); });
              }
            }
          }
          else if ( !Xinha.is_ie )
          {
            value = root.style.cssText.replace(/rgb\(.*?\)/ig,function(rgb){ return Xinha._colorToRgb(rgb) });
          }
          else if (!value) // IE8 has style in attributes (see below), but it's empty! 
          {
            continue;
          }
          if ( /^(_moz)?$/.test(value) )
          {
            // Mozilla reports some special tags
            // here; we don't need them.
            continue;
          }
          html.push(" " + name + '="' + Xinha.htmlEncode(value) + '"');
        }
        //IE fails to put style in attributes list & cssText is UPPERCASE
        if ( Xinha.is_ie && root.style.cssText )
        {
          html.push(' style="' + root.style.cssText.replace(/(^)?([^:]*):(.*?)(;|$)/g, function(m0, m1,m2,m3, m4){return m2.toLowerCase() + ':' + m3 + m4;}) + '"');
        }
        if ( Xinha.is_ie && root.tagName.toLowerCase() == "option" && root.selected )
        {
          html.push(' selected="selected"');
        }
        if ( html !== [] )
        {
          if ( closed && root_tag=="p" )
          {
            //never use <p /> as empty paragraphs won't be visible
            // html.push(">&nbsp;</p>");
          }
          else if ( closed )
          {
            //html.push(" />");
          }
          else
          {
            //html.push(">");
          }
        }
      }
      var containsBlock = false;
      if ( root_tag == "script" || root_tag == "noscript" )
      {
        if ( !editor.config.stripScripts )
        {
          if (Xinha.is_ie)
          {
            var innerText = "\n" + root.innerHTML.replace(/^[\n\r]*/,'').replace(/\s+$/,'') + '\n' + indent;
          }
          else
          {
            var innerText = (root.hasChildNodes()) ? root.firstChild.nodeValue : '';
          }
          html.push(innerText + '</'+root_tag+'>' + ((Xinha.is_ie) ? '\n' : ''));
        }
      }
      else if (root_tag == "pre")
      {
        html.push(((Xinha.is_ie) ? '\n' : '') + root.innerHTML.replace(/<br>/g,'\n') + '</'+root_tag+'>');
      }
      else
      {
        for ( i = root.firstChild; i; i = i.nextSibling )
        {
          if ( !containsBlock && i.nodeType == 1 && Xinha.isBlockElement(i) )
          {
            containsBlock = true;
          }
          html = html.concat(_getCode(i, true, editor, indent + '  '));
        }
        if ( outputRoot && !closed )
        {
//          html.push((((Xinha.isBlockElement(root) && containsBlock) || root_tag == 'head' || root_tag == 'html') ? ('\n' + indent) : '') + "</" + root.tagName.toLowerCase() + ">");
        }
      }
    break;

    case 3: // Node.TEXT_NODE
      if ( /^script|noscript|style$/i.test(root.parentNode.tagName) )
      {
        html = [root.data];
      }
      else if(root.data.trim() == '')
      {
        if(root.data)
        {
          html = [' '];
        }
        else
        {
          html = [''];
        }
      }
      else
      {
        html = root.data; //[Xinha.htmlEncode(root.data)];
      }
    break;

    case 8: // Node.COMMENT_NODE
      // html = ["<!--" + root.data + "-->"];
    break;
  }
  return html;
}









// Grabbed from http://kevin.vanzonneveld.net/techblog/article/javascript_equivalent_for_phps_htmlentities/
function htmlentities (string, quote_style) {
    // http://kevin.vanzonneveld.net
    // +   original by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +    revised by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +   improved by: nobbler
    // +    tweaked by: Jack
    // +   bugfixed by: Onno Marsman
    // +    revised by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // -    depends on: get_html_translation_table
    // *     example 1: htmlentities('Kevin & van Zonneveld');
    // *     returns 1: 'Kevin &amp; van Zonneveld'
    // *     example 2: htmlentities("foo'bar","ENT_QUOTES");
    // *     returns 2: 'foo&#039;bar'
 
    var histogram = {}, symbol = '', tmp_str = '', entity = '';
    tmp_str = string.toString();
    
    if (false === (histogram = get_html_translation_table('HTML_ENTITIES', quote_style))) {
        return false;
    }
    
    for (symbol in histogram) {
        entity = histogram[symbol];
        tmp_str = tmp_str.split(symbol).join(entity);
    }
    
    return tmp_str;
}


// Grabbed from http://kevin.vanzonneveld.net/techblog/article/javascript_equivalent_for_phps_html_entity_decode/
function html_entity_decode( string, quote_style ) {
    // http://kevin.vanzonneveld.net
    // +   original by: john (http://www.jd-tech.net)
    // +      input by: ger
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +    revised by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +   bugfixed by: Onno Marsman
    // +   improved by: marc andreu
    // +    revised by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // -    depends on: get_html_translation_table
    // *     example 1: html_entity_decode('Kevin &amp; van Zonneveld');
    // *     returns 1: 'Kevin & van Zonneveld'
    // *     example 2: html_entity_decode('&amp;lt;');
    // *     returns 2: '&lt;'
 
    var histogram = {}, symbol = '', tmp_str = '', entity = '';
    tmp_str = string.toString();
    
    if (false === (histogram = get_html_translation_table('HTML_ENTITIES', quote_style))) {
        return false;
    }
 
    // &amp; must be the last character when decoding!
    delete(histogram['&']);
    histogram['&'] = '&amp;';
 
    for (symbol in histogram) {
        entity = histogram[symbol];
        tmp_str = tmp_str.split(entity).join(symbol);
    }
    
    return tmp_str;
}


// Grabbed from http://kevin.vanzonneveld.net/techblog/article/javascript_equivalent_for_phps_get_html_translation_table/

function get_html_translation_table(table, quote_style) {
    // http://kevin.vanzonneveld.net
    // +   original by: Philip Peterson
    // +    revised by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +   bugfixed by: noname
    // +   bugfixed by: Alex
    // +   bugfixed by: Marco
    // +   bugfixed by: madipta
    // %          note: It has been decided that we're not going to add global
    // %          note: dependencies to php.js. Meaning the constants are not
    // %          note: real constants, but strings instead. integers are also supported if someone
    // %          note: chooses to create the constants themselves.
    // %          note: Table from http://www.the-art-of-web.com/html/character-codes/
    // *     example 1: get_html_translation_table('HTML_SPECIALCHARS');
    // *     returns 1: {'"': '&quot;', '&': '&amp;', '<': '&lt;', '>': '&gt;'}
    
    var entities = {}, histogram = {}, decimal = 0, symbol = '';
    var constMappingTable = {}, constMappingQuoteStyle = {};
    var useTable = {}, useQuoteStyle = {};
    
    useTable      = (table ? table.toUpperCase() : 'HTML_SPECIALCHARS');
    useQuoteStyle = (quote_style ? quote_style.toUpperCase() : 'ENT_COMPAT');
    
    // Translate arguments
    constMappingTable[0]      = 'HTML_SPECIALCHARS';
    constMappingTable[1]      = 'HTML_ENTITIES';
    constMappingQuoteStyle[0] = 'ENT_NOQUOTES';
    constMappingQuoteStyle[2] = 'ENT_COMPAT';
    constMappingQuoteStyle[3] = 'ENT_QUOTES';
    
    // Map numbers to strings for compatibilty with PHP constants
    if (!isNaN(useTable)) {
        useTable = constMappingTable[useTable];
    }
    if (!isNaN(useQuoteStyle)) {
        useQuoteStyle = constMappingQuoteStyle[useQuoteStyle];
    }
 
    if (useTable == 'HTML_SPECIALCHARS') {
        // ascii decimals for better compatibility
        entities['38'] = '&amp;';
        if (useQuoteStyle != 'ENT_NOQUOTES') {
            entities['34'] = '&quot;';
        }
        if (useQuoteStyle == 'ENT_QUOTES') {
            entities['39'] = '&#039;';
        }
        entities['60'] = '&lt;';
        entities['62'] = '&gt;';
    } else if (useTable == 'HTML_ENTITIES') {
        // ascii decimals for better compatibility
      entities['38']  = '&amp;';
        if (useQuoteStyle != 'ENT_NOQUOTES') {
            entities['34'] = '&quot;';
        }
        if (useQuoteStyle == 'ENT_QUOTES') {
            entities['39'] = '&#039;';
        }
      entities['60']  = '&lt;';
      entities['62']  = '&gt;';
      entities['160'] = '&nbsp;';
      entities['161'] = '&iexcl;';
      entities['162'] = '&cent;';
      entities['163'] = '&pound;';
      entities['164'] = '&curren;';
      entities['165'] = '&yen;';
      entities['166'] = '&brvbar;';
      entities['167'] = '&sect;';
      entities['168'] = '&uml;';
      entities['169'] = '&copy;';
      entities['170'] = '&ordf;';
      entities['171'] = '&laquo;';
      entities['172'] = '&not;';
      entities['173'] = '&shy;';
      entities['174'] = '&reg;';
      entities['175'] = '&macr;';
      entities['176'] = '&deg;';
      entities['177'] = '&plusmn;';
      entities['178'] = '&sup2;';
      entities['179'] = '&sup3;';
      entities['180'] = '&acute;';
      entities['181'] = '&micro;';
      entities['182'] = '&para;';
      entities['183'] = '&middot;';
      entities['184'] = '&cedil;';
      entities['185'] = '&sup1;';
      entities['186'] = '&ordm;';
      entities['187'] = '&raquo;';
      entities['188'] = '&frac14;';
      entities['189'] = '&frac12;';
      entities['190'] = '&frac34;';
      entities['191'] = '&iquest;';
      entities['192'] = '&Agrave;';
      entities['193'] = '&Aacute;';
      entities['194'] = '&Acirc;';
      entities['195'] = '&Atilde;';
      entities['196'] = '&Auml;';
      entities['197'] = '&Aring;';
      entities['198'] = '&AElig;';
      entities['199'] = '&Ccedil;';
      entities['200'] = '&Egrave;';
      entities['201'] = '&Eacute;';
      entities['202'] = '&Ecirc;';
      entities['203'] = '&Euml;';
      entities['204'] = '&Igrave;';
      entities['205'] = '&Iacute;';
      entities['206'] = '&Icirc;';
      entities['207'] = '&Iuml;';
      entities['208'] = '&ETH;';
      entities['209'] = '&Ntilde;';
      entities['210'] = '&Ograve;';
      entities['211'] = '&Oacute;';
      entities['212'] = '&Ocirc;';
      entities['213'] = '&Otilde;';
      entities['214'] = '&Ouml;';
      entities['215'] = '&times;';
      entities['216'] = '&Oslash;';
      entities['217'] = '&Ugrave;';
      entities['218'] = '&Uacute;';
      entities['219'] = '&Ucirc;';
      entities['220'] = '&Uuml;';
      entities['221'] = '&Yacute;';
      entities['222'] = '&THORN;';
      entities['223'] = '&szlig;';
      entities['224'] = '&agrave;';
      entities['225'] = '&aacute;';
      entities['226'] = '&acirc;';
      entities['227'] = '&atilde;';
      entities['228'] = '&auml;';
      entities['229'] = '&aring;';
      entities['230'] = '&aelig;';
      entities['231'] = '&ccedil;';
      entities['232'] = '&egrave;';
      entities['233'] = '&eacute;';
      entities['234'] = '&ecirc;';
      entities['235'] = '&euml;';
      entities['236'] = '&igrave;';
      entities['237'] = '&iacute;';
      entities['238'] = '&icirc;';
      entities['239'] = '&iuml;';
      entities['240'] = '&eth;';
      entities['241'] = '&ntilde;';
      entities['242'] = '&ograve;';
      entities['243'] = '&oacute;';
      entities['244'] = '&ocirc;';
      entities['245'] = '&otilde;';
      entities['246'] = '&ouml;';
      entities['247'] = '&divide;';
      entities['248'] = '&oslash;';
      entities['249'] = '&ugrave;';
      entities['250'] = '&uacute;';
      entities['251'] = '&ucirc;';
      entities['252'] = '&uuml;';
      entities['253'] = '&yacute;';
      entities['254'] = '&thorn;';
      entities['255'] = '&yuml;';
    } else {
        throw Error("Table: "+useTable+' not supported');
        return false;
    }
    
    // ascii decimals to real symbols
    for (decimal in entities) {
        symbol = String.fromCharCode(decimal);
        histogram[symbol] = entities[decimal];
    }
    
    return histogram;
}


})();