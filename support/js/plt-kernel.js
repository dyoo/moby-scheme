// Some of this code comes from "Javascript: the Good Parts, 1st Edition."







if (typeof Object.create !== 'function') {
     Object.create = function (o) {
         var F = function () {};
         F.prototype = o;
         return new F();
     };
}



// Core classes
// Struct: implemented by all structures
// Equatable: implemented by things that can be compared by equal?



// Fixme: figure out how to do packages properly.
var org = {};
org.plt = {};
org.plt.Kernel = {
  Struct: function () {
  },
    
  isStruct: function(thing) {
    return thing instanceof this.Struct;
  },

  
  
  isEqual : function(x, y) {
    if ("isEqual" in x) {
      return x.isEqual(y);
    } else if ("isEqual" in y) {
      return y.isEqual(x);
    } else {
      return x == y;
    }
  },

  
  identity : function (x){
    return x;
  },

  
  add : function(x) {
    var i, sum = 0;
    for(i = 0; i < arguments.length; i++) {
      sum += arguments[i];
    }
    return sum;
  }
  
  
  
  
   
};
