load("support.js");


callcount = 0;
breakat = 10;
breakevery = 10;

pause = function(){
	if(verbose)
		print("PAUSING at " + callcount);

	breakat = callcount + breakevery;

	if(verbose)
		print("next pause at "+ breakat);

	return Continuation.CWCC (function(cont){
								return null;});
};

// tak FRAME 0

tak_frame0 = function(x,y,z){
	this.x = x;
	this.y = y;
	this.z = z;
};

tak_frame0.prototype = new ContinuationFrame;
tak_frame0.prototype.Invoke = function(return_value){
	return tak_an0(this.x,this.y,this.z);
};

tak_frame0.prototype.print = function(){
	print("tak Frame 0: x = " + this.x
			 + " - y = " + this.y
			 + " - z = " + this.z);
};

// tak_AN

tak_an = function(x,y,z){
	if(debug)
		print("tak_an : x = " + x + "  y = " + y + "  z = " + z);
	callcount+=1;
	if(callcount == breakat){
		try{
			pause();
		}catch(sce){
			if(verbose)
				print("-- tak_an: caught a SCE!");
			sce.Extend(new tak_frame0(x,y,z));
			throw sce;
		}
	}
	return tak_an0(x,y,z);
};

// tak FRAME 1

tak_frame1 = function(x,y,z){
	this.x = x;
	this.y = y;
	this.z = z;
};

tak_frame1.prototype = new ContinuationFrame;
tak_frame1.prototype.Invoke = function(return_value){
	return tak_an1(return_value, this.x, this.y, this.z);
};

// tak_AN0

tak_an0 = function(x,y,z){
	if(debug)
		print("tak_an0 : x = " + x + "  y = " + y + "  z = " + z);
	if(y>=x)
		return z;
	var temp0;
	try
	{
		temp0 = tak_an(x-1,y,z);
	}catch(sce)
	{
		if(verbose)
			print("tak an0: caught!");
		sce.Extend(new tak_frame1(x,y,z));
		throw sce;
	}
	return tak_an1(temp0, x, y, z);
};

// tak_AN1

tak_an1 = function(temp0, x, y, z){
	if(debug)
		print("tak_an1 : x = " + x 
				+ "  y = " + y 
				+ "  z = " + z 
				+ " temp0 = " + temp0);
	var temp1;
	try
	{
		temp1 = tak_an(y-1,z,x);
	}catch(sce)
	{
		if(verbose)
			print("tak an1: caught!");
		sce.Extend(new tak_frame2(temp0,x,y,z));
		throw sce;
	}
	return tak_an2(temp1,temp0,x,y,z);
};

// tak FRAME 2

tak_frame2 = function(temp0,x,y,z){
	this.temp0 = temp0;
	this.x = x;
	this.y = y;
	this.z = z;
};

tak_frame2.prototype = new ContinuationFrame;
tak_frame2.prototype.Invoke = function(return_value){
	return tak_an2(return_value, this.temp0, this.x, this.y, this.z);
};

// tak_AN2

tak_an2 = function(temp1,temp0,x,y,z){
	if(debug)
		print("tak_an2 : x = " + x 
				+ "  y = " + y 
				+ "  z = " + z 
				+ " temp0 = " + temp0 
				+ " temp1 = " + temp1);
	var temp2;
	try
	{
		temp2 = tak_an(z-1,x,y);
	}catch(sce)
	{
		if(verbose)
			print("tak an1: caught!");
		sce.Extend(new tak_frame3(temp1,temp0));
		throw sce;
	}
	return tak_an3(temp2,temp1,temp0);
};

// tak FRAME 3

tak_frame3 = function(temp1,temp0){
	this.temp1 = temp1;
	this.temp0 = temp0;
};

tak_frame3.prototype = new ContinuationFrame;
tak_frame3.prototype.Invoke = function(return_value){
	return tak_an3(return_value, this.temp1, this.temp0);
};

// tak_AN3

tak_an3 = function(temp2, temp1, temp0){
	return tak_an(temp0,temp1,temp2);
};

//////////////////////////////////////////////////////////////////////
// BELOW FOR TESTING

tak = function(x,y,z){
	// resetting the control variables
	// these will be changed during the execution
	callcount = 0;
	breakat = 10;

	printContinuation = false;

	return Continuation.EstablishInitialContinuation(function(){return tak_an(x,y,z);});
};

test = function(){
    var startTime = new Date();
    var result = tak(1800,1200,600);
    print(result);
    var endTime = new Date();
    print("Elapsed time : " + startTime-endTime);
    return result;
};

		print("--Script loaded--\n"
			 + "Initial CallCount: " + callcount
			 +"\nBreakAt: " + breakat
			 + "\nVerbosity: " + verbose
			 + "\nLoop Verbosity: " + loopverbose
			 + "\nDebug Mode: " + debug
			 + "\nContinuation print: " + printContinuation);
