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
}