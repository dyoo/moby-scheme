package org.plt;
import org.plt.Kernel;
import org.plt.types.*;
import org.junit.Test;
import static org.junit.Assert.*;

public class TestKernel {

    @Test public void testEmpty() {
	assertTrue(Kernel.empty_question_(Empty.EMPTY).isTrue());
    }

    // Fill me in with more test cases!
    @Test public void testTan(){
    	assertTrue(Kernel._equal_(Kernel.tan(Rational.ZERO), Rational.ZERO).isTrue());
    }
    
    @Test public void testSinh(){
    	assertTrue(Kernel._equal_(Kernel.sinh(Rational.ZERO), Rational.ZERO).isTrue());
    }
    
    @Test public void testCosh(){
    	assertTrue(Kernel._equal_(Kernel.cosh(Rational.ZERO), Rational.ONE).isTrue());
    }
    
    @Test public void testBoolean_question_(){
    	assertTrue(Kernel.boolean_question_(Logic.TRUE).isTrue());
    }
    
    
}