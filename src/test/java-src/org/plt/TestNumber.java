package org.plt;

import org.plt.Kernel;
import org.plt.types.*;
import org.junit.Test;
import static org.junit.Assert.*;

public class TestNumber {
    @Test public void testNumberToString() {
	assertEquals(new Rational(10, 1).toString(), "10");
	assertEquals(FloatPoint.fromString("3.14").ceiling().toString(), "4");
	assertEquals(FloatPoint.fromString("2.718").floor().toString(), "2");
    }


}