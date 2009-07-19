// Platform-specific stuff.
var plt = plt || {};

plt.lib = {};


(function () {
    function toNum(x) {
	return plt.types.Rational.makeInstance(x, 1);
    }
    plt.lib.Telephony = {};
    plt.lib.Telephony.getSignalStrengths = function() {
	var result = plt.types.Empty.EMPTY;
	if (typeof Device != 'undefined') {
	    var infos = Device.getSignalStrengths();
	    for (var i = 0; i < infos.length; i++) {
		var info = infos.get(i);
		result = plt.types.Cons.makeInstance(
		    plt.Kernel.list([toNum(info.getId()),
				     toNum(info.getStrength())]),
		    result);
	    }
	}
	return result;
    };
}());