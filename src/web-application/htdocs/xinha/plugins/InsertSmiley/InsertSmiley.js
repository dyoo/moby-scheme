/*---------------------------------------*\
 Insert Smiley Plugin for Xinha
 -----------------------------------------
 author: Ki Master George
 e-mail: kimastergeorge@gmail.com
\*---------------------------------------*/


function InsertSmiley(editor) {
  this.editor = editor;
  var cfg = editor.config;
  var self = this;

  // register the toolbar buttons provided by this plugin
  cfg.registerButton({
    id       : "insertsmiley",
    tooltip  : this._lc("Insert Smiley"),
    image    : editor.imgURL("ed_smiley.gif", "InsertSmiley"),
    textMode : false,
    action   : function(editor) {
                 self.buttonPress(editor);
               }
  });
  cfg.addToolbarElement("insertsmiley", "inserthorizontalrule", 1);
}

(function() {
    function getElementsByClass(theClass) {
	var elts = [];
	//Create Array of All HTML Tags
	var allHTMLTags=document.getElementsByTagName("*");

	//Loop through all tags using a for loop
	for (i=0; i<allHTMLTags.length; i++) {

	    //Get all tags with the specified class name.
	    if (allHTMLTags[i].className==theClass) {
		elts.push(allHTMLTags[i]);
	    }
	}
	return elts; 
    }


    InsertSmiley._pluginInfo = {
	name          : "InsertSmiley",
	version       : "1.0",
	developer     : "Ki Master George",
	developer_url : "http://kimastergeorge.i4host.com/",
	c_owner       : "Ki Master George",
	sponsor       : "Ki Master George",
	sponsor_url   : "http://kimastergeorge.i4host.com/",
	license       : "htmlArea"
    };


    var placeholderImg = '<img class="InsertSmiley_placeholder" src="'+
	Xinha.getPluginDir("InsertSmiley")+'/img/ed_smiley.gif" />';


    InsertSmiley.prototype._lc = function(string) {
	return Xinha._lc(string, 'InsertSmiley');
    };



    InsertSmiley.prototype.onGenerate = function() {
	// Fill me in.
    };



    InsertSmiley.prototype.inwardHtml = function(html)
    {
	return html;
    };
    

    InsertSmiley.prototype.outwardHtml = function(html)
    {
	return html;
    };


    InsertSmiley.prototype.buttonPress = function(editor) {
	var self = this;
	var img = new Image();
	img.src = editor.imgURL("ed_smiley.gif", "InsertSmiley");
	img.setAttribute("class", "InsertSmiley_placeholder");
	editor.insertNodeAtSelection(img);
    };

})();