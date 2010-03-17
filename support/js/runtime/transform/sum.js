load("support.js");


callcount = 0;
breakat = 10;
breakevery = 10;

sum_pause = function(){
	if(verbose)
		print("PAUSING at " + callcount);

	breakat = callcount + breakevery;

	if(verbose)
		print("next pause at "+ breakat);

	return Continuation.CWCC (function(cont){
								return null;});
};

// SUM FRAME 0

// int n, int acc
sum_frame0 = function(n, acc){
	this.n = n;
	this.acc = acc;
};

sum_frame0.prototype = new ContinuationFrame;
sum_frame0.prototype.Invoke = function(return_value){
	return sum_an0(this.n,this.acc);
};

sum_frame0.prototype.print = function(){
	print("Sum Frame 0: n = " + this.n + " - acc = " + this.acc);
};


// SUM_AN

sum_an = function(n, acc){
	if(debug)
		print("sum_an : n = " + n + "  acc = " + acc);
	callcount+=1;
	if(callcount == breakat){
		try{
			sum_pause();
		}catch(sce){
			if(verbose)
				print("-- Sum_an: caught a SCE!");
			sce.Extend(new sum_frame0(n,acc));
			throw sce;
		}
	}
	return sum_an0(n,acc);
};

// SUM FRAME 1

sum_frame1 = function(n,acc){
	this.n = n;
	this.acc = acc;
};

sum_frame1.prototype = new ContinuationFrame;
sum_frame1.prototype.Invoke = function(return_value){
	return sum_an1(return_value, this.n, this.acc);
};

// SUM_AN0

sum_an0 = function(n,acc){
	if(debug)
		print("sum_an0 : n = " + n + "  acc = " + acc);
	if(n<=0)
		return acc;
	var temp0;
	try
	{
		temp0 = n-1;
	}catch(sce)
	{
		if(verbose)
			print("sum an0: caught!");
		sce.Extend(new sum_frame1(n,acc));
		throw sce;
	}
	return sum_an1(temp0, n, acc);
};

// SUM_AN1

sum_an1 = function(temp0, n, acc){
	if(debug)
		print("sum_an1 : n = " + n + "  acc = " + acc + " temp0 = " + temp0);
	var temp1;
	try
	{
		temp1 = n+acc;
	}catch(sce)
	{
		if(verbose)
			print("sum an1: caught!");
		sce.Extend(new sum_frame2(temp0));
		throw sce;
	}
	return sum_an2(temp0,temp1);
};

// SUM FRAME 2

sum_frame2 = function(temp0){
	this.temp0 = temp0;
};

sum_frame2.prototype = new ContinuationFrame;
sum_frame2.prototype.Invoke = function(return_value){
	return sum_an2(this.temp0,return_value);
};

// SUM_AN2

sum_an2 = function(temp0,temp1){
	return sum_an(temp0,temp1);
};

//////////////////////////////////////////////////////////////////////
// BELOW FOR TESTING


sum = function(n,acc){
	// resetting the control variables
	// these will be changed during the execution
	callcount = 0;
	breakat = 10;
	return Continuation.EstablishInitialContinuation(function(){return sum_an(n,acc);});
};

test = function(){
	var startTime = new Date();
	print(sum(1000000,0));
	var endTime = new Date();
	print("Elapsed time : " + startTime-endTime);
};


	print("--Script loaded--\n"
			 + "Initial CallCount: " + callcount
			 +"\nBreakAt: " + breakat
			 + "\nVerbosity: " + verbose
			 + "\nLoop Verbosity: " + loopverbose
			 + "\nDebug Mode: " + debug
			 + "\nContinuation print: " + printContinuation);
