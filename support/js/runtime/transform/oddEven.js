// Mutual recursion example

load("support.js");

callcount = 0;
breakat = 11;
breakevery = 11;


function nullFunction(cont) {
    return null;
}

pause = function(){
    breakat = callcount + breakevery;

    return Continuation.CWCC(nullFunction);
};

// EVEN FRAME 0

// int n
even_question_frame0 = function(n){
    this.n = n;
};

even_question_frame0.prototype = new ContinuationFrame;
even_question_frame0.prototype.Invoke = function(return_value){
    return even_question_an0(this.n);
};

even_question_frame0.prototype.print = function(){
    print("even_question Frame 0: n = " + this.n);
};



// EVEN_AN

even_question_an = function(n){
    callcount+=1;
    if(callcount == breakat){
	try{
	    pause();
	}catch(sce){
	    sce.Extend(new even_question_frame0(n));
	    throw sce;
	}
    }
    return even_question_an0(n);
};

// even_question FRAME 1

even_question_frame1 = function(n){
    this.n = n;
};

even_question_frame1.prototype = new ContinuationFrame;
even_question_frame1.prototype.Invoke = function(return_value){
    return even_question_an1(return_value);
};

// even_question_AN0

even_question_an0 = function(n){
    if(n==0)
		return true;
    var temp0;
    try
    {
		temp0 = n-1;
    }catch(sce)
    {
	sce.Extend(new even_question_frame1(n));
	throw sce;
    }
    return even_question_an1(temp0);
};

// even_question_AN1

even_question_an1 = function(temp0){
	return odd_question_an(temp0);
};

// ODD FRAME 0

// int n
odd_question_frame0 = function(n){
    this.n = n;
};

odd_question_frame0.prototype = new ContinuationFrame;
odd_question_frame0.prototype.Invoke = function(return_value){
    return odd_question_an0(this.n);
};

odd_question_frame0.prototype.print = function(){
    print("odd_question Frame 0: n = " + this.n);
};


// odd_AN

odd_question_an = function(n){
    callcount+=1;
    if(callcount == breakat){
	try{
	    pause();
	}catch(sce){
	    sce.Extend(new odd_question_frame0(n));
	    throw sce;
	}
    }
    return odd_question_an0(n);
};

// odd_question FRAME 1

odd_question_frame1 = function(n){
    this.n = n;
};

odd_question_frame1.prototype = new ContinuationFrame;
odd_question_frame1.prototype.Invoke = function(return_value){
    return odd_question_an1(return_value);
};

// odd_question_AN0

odd_question_an0 = function(n){
    if(n==0)
		return false;
    var temp0;
    try
    {
		temp0 = n-1;
    }catch(sce)
    {
	sce.Extend(new odd_question_frame1(n));
	throw sce;
    }
    return odd_question_an1(temp0, n);
};

// odd_question_AN1

odd_question_an1 = function(temp0){
	return even_question_an(temp0);
};


//////////////////////////////////////////////////////////////////////
// BELOW FOR TESTING


even_question_ = function(n){
    // resetting the control variables
    // these will be changed during the execution
    callcount = 0;
    breakat = 11;
    return Continuation.EstablishInitialContinuation(function(){return even_question_an(n);});
};

odd_question_ = function(n,acc){
    // resetting the control variables
    // these will be changed during the execution
    callcount = 0;
    breakat = 11;
    return Continuation.EstablishInitialContinuation(function(){return odd_question_an(n);});
};

test = function(){
    var startTime = new Date();
    var result = even_question_(1000000,0);
    print(result);
    var endTime = new Date();
    print("Elapsed time : " + startTime-endTime);
    return result;
};


print("--Script loaded--\n"
      + "Initial CallCount: " + callcount
      +"\nBreakAt: " + breakat);
