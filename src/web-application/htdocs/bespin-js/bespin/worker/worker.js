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

// = Web Workers / Gears WorkerPool Abstraction =

dojo.provide("bespin.worker.worker");

(function() {
var WORKER_COUNT = 1;
var WORKER_INDEX = 0;
var CALL_INDEX   = 0;
var USE_GEARS    = false;

// these functions are part of a hack to transport code into the worker via
// a hash (#) url part that is extracted and evaled inside the worker
// Sounds like a security hole, but maybe it is not.
var JS_WORKER_SOURCE = "js/bespin/bootstrap_worker.js";
var uriEncodeSource = function(source) {
    return JS_WORKER_SOURCE+"#"+escape(source)
};
var uriDecodeSource = function(uri) {
    return unescape(uri.substr( (JS_WORKER_SOURCE+"#").length ))
};

// If there is no Worker API (http://www.whatwg.org/specs/web-workers/current-work/) yet,
// try to build one using Google Gears API
if (typeof Worker == "undefined") {
    BespinGearsInitializeGears(); // this functions initializes Gears only if we need it
    if(window.google && google.gears) {
        USE_GEARS   = true; // OK, gears is here

        var wp      = google.gears.factory.create('beta.workerpool');
        var workers = {};
        Worker      = function(uri, source) { // The worker class, non standard second source para
            this.isGears = true;

            this.id = wp.createWorker(source);
            workers[this.id] = this;
        };

        Worker.prototype = { // we can post messages to the worker
            postMessage: function(data) {
                wp.sendMessage(data, this.id)
            }
        };

        // upon receiving a message we call our onmessage callback
        // DOM-Message-Events are not supported
        wp.onmessage = function(a, b, message) {
            var worker = workers[message.sender];
            var cb = worker.onmessage;
            if(cb) {
                cb.call(worker, {
                    data: message.body
                });
            }
        };
    }
}
//** {{{ bespin.worker.Worker }}} **
//
// Takes an objects and build a facade object for it.
// Creates source code for a web worker that implements the same functionality as the original object
// and sends the source code to the web worker (The source code is created recursively, so complex
// objects are supported).
// Methods which are send to the facade are from now one delegated to the worker.
//
// Because all method calls are now async, the methods of the facade object return an object
// that has a function property called "and" which can invoked to defined a callback that will receive
// the return value of the method call.
// The and function takes 4 parameters
//    context: the this value of the callback.
//    mutex: (optional) a mutex for the callback (Means that after-callback of the mutex wont be called until after this callback is finished
//    paras: (optiona) array of extra parameters for the callback.
//    callback: Callback for the method. First parameter will be the return value of the method
//
// Background APIs:
// We prefer to use the web worker API
// If that is not there we try to use Gears Workers
// If that is not there code will be executed within the regular context but still be async (using setTimeout)
//
// Limitations:
// Objects may not include
// - closures
// - references to DOM nodes
// - circular references

dojo.declare("bespin.worker.WorkerFacade", null, {
    constructor: function(obj, workerCount, libs) { // only use workerCount > 1 if the object is stateless

        // Properties use __name__ notation to avoid conflicts with facade methods

        this.__obj__ = obj;

        var callbacks = {};
        this.__callbacks__ = callbacks;

        this.__workerCount__ = workerCount || WORKER_COUNT;

        // __hasWorkers__ is a public API of the facade
        this.__hasWorkers__ = false;

        if(typeof Worker != "undefined") { // We have a Worker implementation
            this.__hasWorkers__ = true;

            var source  = this.createWorkerSource(obj, libs);
            var workers = this.createWorkers(source);
            this.__workers__ = workers;
        }

        this.createFacade(obj);

    },

    // We support pools of workers which share the load
    __getWorker__: function() {
        var index = WORKER_INDEX++ % this.__workerCount__; // round robin scheduling
        // TODO maintain a smarter queue based on which workers are actually idle
        return this.__workers__[index];
    },

    // Create N workers based on source
    createWorkers: function(source) { // round robin scheduling
        var self    = this;
        var workers = [];

        // The standard callback choose a callback for the particular method using
        // the callIndex that is set upon sending the method
        var cb = function(event) {
            var data  = event.data;
            if(typeof data == "string") {
                data = dojo.fromJson(data);
            }
            var index = data.callIndex;

            var callback = self.__callbacks__[index];
            delete self.__callbacks__[index];
            if(callback) {
                callback(data.returnValue);
            }
        };

        var loadScript = function (index, url) {
            var worker = this;
            bespin.get("server").request('GET', url, null, {
                onSuccess: function (src) {
                    worker.postMessage("__IMPORT_SCRIPT__//"+index+"\n"+src);
                }
            });
        }

        var onmessage = function(event) {
            var message = event.data
            if(typeof message == "string") {
                if(message.indexOf("log=") == 0) {
                    console.log("From Worker: "+message.substr(4))
                    return
                }
                else
                if(message.indexOf("__IMPORT_SCRIPT__") == 0) {
                    var json = message.substr("__IMPORT_SCRIPT__".length)
                    var paras = dojo.fromJson(json)
                    loadScript.apply(this, paras)
                    return
                }
                else {
                    message = dojo.fromJson(message)
                }
            }
            cb.call(this, event)

        }

        for(var i = 0; i < this.__workerCount__;i++) {
            //console.log("Create worker")
            var worker = new Worker("/js/bespin/bootstrap_worker.js", source);
            //console.log("Worker created")
            worker.onmessage = onmessage;
            source = "// YOUcannotGuessMe\n" + source
            if(!USE_GEARS) {
                window.setTimeout(function() {
                    worker.postMessage(source)
                },0)
            }
            workers.push(worker)
        }
        return workers
    },

    // create a shallow facade for object
    createFacade: function(obj) {

        var facade = this;

        for(var prop in obj) {
            if(prop.charAt(0) != "_") { // supposedly we dont need "private" methods. Delete if assumption is wrong
                (function() { // make a lexical scope
                    var val    = obj[prop];
                    var method = prop
                    if(typeof val == "function") { // functions are replaced with code to call the worker
                        facade[prop] = function() {
                             var self  = this;
                             var index = CALL_INDEX++ // each call gets a globally unique index
                             var paras = Array.prototype.slice.call(arguments);
                             if(this.__hasWorkers__) {
                                 var data = {
                                     callIndex: index,
                                     method: method,
                                     paras:  paras
                                 }
                                 if(!USE_GEARS) data = dojo.toJson(data) // here we should really test whether our postMessage supports structured data. Safari 4 does not
                                 // send the method to a worker
                                 // console.log("Contacting worker "+data)
                                 this.__getWorker__().postMessage(data)
                             } else {
                                 // No worker implementation available. Use an async call using
                                 // setTimeout instead
                                 var self = this;
                                 window.setTimeout(function() {
                                     var retVal = self.__obj__[method].apply(self.__obj__, paras);
                                     var callback = self.__callbacks__[index];
                                     delete self.__callbacks__[index]
                                     if(callback) {
                                         callback(retVal)
                                     }
                                 }, 0)
                             }
                             // Return an object to create a "fluid-interface" style callback generator
                             // callback will be applied against context
                             // callback will be part of the mutex
                             // paras is an array of extra paras for the callback
                             return {
                                 and: function(context, mutex, paras, callback) {
                                     var func = arguments[arguments.length - 1] // always the last para
                                     if(mutex instanceof bespin.worker.Mutex) {
                                         mutex.start()
                                         func = function() {
                                             callback.apply(this, arguments)
                                             mutex.stop()
                                         }
                                     }

                                     self.__callbacks__[index] = function() {
                                         paras = Array.prototype.slice.call(arguments).concat(paras)
                                         func.apply(context, paras)
                                     }
                                 }
                             }
                        }
                    }
                    else {
                        // put instance vars here, too?
                    }
                })()
            }
        }
    },

    // Determines whether there are functions (deeply) inside a JS object
    hasFunctions: function(obj) {
        for(var i in obj) {
            var val = obj[i];
            if(typeof val == "function") {
                return true
            }
            if(val && typeof val == "object") {
                if(this.hasFunctions(val)) {
                    return true
                }
            }
        }
        return false
    },

    // Recursively turn a JS object into its source including functions
    serializeToPortableSource: function(obj) {
        var self   = this;
        var source = "{\n"

        for(var prop in obj) {
            // console.log("Serializing "+prop);
            (function() { // lexical scope
                if(prop == "_constructor") { // workaround for unserializable method in dojo
                    return                   // maybe replace with test for [native code] in string
                }
                var val    = obj[prop];
                var method = prop
                var src = ""
                if(typeof val == "function") { // serialize function to their string representation
                    src = val.toString();      // toSource() might be better but toString insert nice line breaks
                }
                // if val is an object that included functions we need to call ourselves recursively
                else if(val && typeof val == "object" && self.hasFunctions(val)) {
                    src = self.serializeToPortableSource(val)
                }
                // everything else is turned into JSON
                else {
                    src = dojo.toJson(val)
                }

                // Make sure to encode the property so nobody can insert arbitrary string into our JS
                prop = '"'+prop.replace(/"/g, '\\"', 'g')+'"'

                source += prop+": "+src+",\n"
            })()
        }

        source += "}\n";

        return source;
    },

    createWorkerSource: function(obj, libs) {
        var con = function(msg) {
            postMessage("log="+msg)
        }
        var source = "console = { log: "+con.toString()+" };\n"

        if(libs) {
            var quoted = [];
            dojo.forEach(libs, function(lib) {
                quoted.push("'"+lib+"'")
            })
            source += "importScripts("+quoted.join(", ")+");\n"
        }

        source += "var theObject = "+this.serializeToPortableSource(obj)

        // onmessage handler for use inside the worker
        var onmessage  = function(event) {
            var body         = event.data
            //console.log("Received "+body)
            var dataIsString = false;
            if(typeof body == "string") { // if the data is a string, assume that it is JSON
                if(body.indexOf("__IMPORT_SCRIPT__") == 0) {
                    var source = body.substr("__IMPORT_SCRIPT__".length);
                    var match = source.match(/^\/\/(\d+)/)
                    if(match) {
                        var index = parseInt(match[1], 10);
                        __evalScriptFromImport(index, source)

                    }
                    return
                }
                dataIsString = true;
                try {
                    body = JSON.parse(body)
                } catch(e) {
                    throw e+""+body
                }
            }
            var method = body.method;

            var o      = theObject

            // actually call the method
            var ret    = o[body.method].apply(o, body.paras)

            var data   = {
                method: body.method,
                returnValue: ret,
                callIndex: body.callIndex // the original callIndex to find callback
            }

            if(dataIsString) { // If data came as a json string encode data as JSON
                data = JSON.stringify(data)
            }

            //console.log("Sending "+data)
            postMessage(data)
        }

        function ajaxRequest (method, url, data, callback, errorCallback) {

            var request
            if(this.clientHasGears()) {
                request = google.gears.factory.create('beta.httprequest');
            } else {
                request = window.ActiveXObject ? new ActiveXObject("Microsoft.XMLHTTP") : new XMLHttpRequest();
            }
            var dataString    = ""
            if(data) {
                for(var i in data) {
                    dataString += encodeURIComponent(i)+"="+encodeURIComponent(data[i])+"&"
                }
            }
            var theUrl = url;
            if(data && method == "GET") {
                theUrl += "?"+dataString
            }
            request.open(method, theUrl, true);

            request.onreadystatechange = function onreadystatechange () {
                if (request.readyState == 4) {
                    if(request.status >= 200 && request.status < 400) {
                        var res = request.responseText;
                        callback(res)
                    } else {
                        if(errorCallback) {
                            return errorCallback(request)
                        } else {
                            throw new Error("Error fetching url "+theUrl+". Response code: " + request.status + " Response text: "+request.responseText)
                        }
                    }
                }
            };
            if(data && method == "POST") {
                // FIXME determine page encoding instead of always using UTF8
                request.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
                request.send(dataString)
            } else {
                dataString = ""
                request.send(dataString);
            }
        }

        if(USE_GEARS) { // For Gears we need to create a fake postMessage function
            var gearsCB = function(a, b, message) {
                var sender = message.sender
                postMessage = function(data) {
                    wp.sendMessage(data, sender)
                }
                onmessage({ // call the onmessage function defined above
                    data: message.body
                })
            }

            source += "\nvar wp = google.gears.workerPool; wp.onmessage = "+gearsCB.toString()+"\n";

            // emulate importScripts in Gears.
            var importScripts = function importScripts () {
                var global = this;
                var src = "";
                var i = 0;
                var load = function(url, callback) {
                    var request = google.gears.factory.create('beta.httprequest');
                    request.open('GET', url);
                    request.onreadystatechange = function() {
                        if(request.readyState == 4) {
                            if(request.status >= 200 && request.status < 400) {
                                var res = request.responseText;
                                src += res+"\n";
                                callback()
                            } else {
                                throw new Error("Error fetching script "+url+". Response code: " + request.status + " Response text: "+request.responseText)
                            }
                        }
                    };
                    request.send()
                }
                var urls = Array.prototype.splice.call(arguments, 0);
                var loader = function() {
                    var url = urls.shift()
                    if(url) {
                        load(url, loader)
                    } else {
                        global.eval(src)
                    }
                }
                loader()
            }

            source += importScripts.toString();
        }

        source += "\nonmessage = "+onmessage.toString()+"; \n"

        //console.log(source)

        return source
    }

});

//** {{{ bespin.worker.Mutex }}} **
//
// Object that maintains a counter of running workers/async processes.
// Calling after(callback) schedules a function to be called until all
// async processes are finished
//
// Is Mutex the correct term?
dojo.declare("bespin.worker.Mutex", null, {
    constructor: function(name, options) {
        this.name  = name;
        this.count = 0;
        this.afterJobs = [];
        this.options   = options || {}
    },
    start: function() {
        this.count = this.count + 1
    },
    stop: function() {
        this.count = this.count - 1
        if(this.count == 0) {
            if(this.options.onlyLast) {
                var last = this.afterJobs[this.afterJobs.length-1];
                if(last) {
                    last()
                }
            } else {
                for(var i = 0; i < this.afterJobs.length; ++i) {
                    var job = this.afterJobs[i];
                    job()
                }
            }
            this.afterJobs = []
        }
    },
    after: function(context, func) {
        this.afterJobs.push(function() {
            func.call(context)
        })
    }
})

// Copyright 2007, Google Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//  1. Redistributions of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//  3. Neither the name of Google Inc. nor the names of its contributors may be
//     used to endorse or promote products derived from this software without
//     specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
// EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Sets up google.gears.*, which is *the only* supported way to access Gears.
//
// Circumvent this file at your own risk!
//
// In the future, Gears may automatically define google.gears.* without this
// file. Gears may use these objects to transparently fix bugs and compatibility
// issues. Applications that use the code below will continue to work seamlessly
// when that happens.

// Sorry Google for modifying this :)
function BespinGearsInitializeGears() {
  // We are already defined. Hooray!
  if (window.google && google.gears) {
    return;
  }

  var factory = null;

  // Firefox
  if (typeof GearsFactory != 'undefined') {
    factory = new GearsFactory();
  } else {
    // IE
    try {
      factory = new ActiveXObject('Gears.Factory');
      // privateSetGlobalObject is only required and supported on WinCE.
      if (factory.getBuildInfo().indexOf('ie_mobile') != -1) {
        factory.privateSetGlobalObject(this);
      }
    } catch (e) {
      // Safari
      if (navigator.mimeTypes["application/x-googlegears"]) {
        factory = document.createElement("object");
        factory.style.display = "none";
        factory.width = 0;
        factory.height = 0;
        factory.type = "application/x-googlegears";
        document.documentElement.appendChild(factory);
      }
    }
  }

  // *Do not* define any objects if Gears is not installed. This mimics the
  // behavior of Gears defining the objects in the future.
  if (!factory) {
    return;
  }

  // Now set up the objects, being careful not to overwrite anything.
  //
  // Note: In Internet Explorer for Windows Mobile, you can't add properties to
  // the window object. However, global objects are automatically added as
  // properties of the window object in all browsers.
  if (!window.google) {
    google = {};
  }

  if (!google.gears) {
    google.gears = {factory: factory};
  }
}

})()

