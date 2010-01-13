package org.plt.moby;
import java.io.IOException;

// FIXME: make these real unit tests.
public class TestStandaloneCompiler {
    private static StandaloneCompiler getCompiler() throws IOException {
	System.out.println("Instantiating a Standalone Compiler");
	StandaloneCompiler c = new StandaloneCompiler();
	System.out.println("Done instantiation");
	return c;
    }

    public static void main(String[] args) throws IOException {


	StandaloneCompiler c = getCompiler();
	CompilationResult r = c.compile("(define (f x a b) (* x x)) (f 42 0 1)  (js-big-bang 0 (on-location-change f))");
	System.out.println(r.getJs());
	System.out.println(r.getPerms());


	r = c.compile("(define (g y) (+ y 42)) (g 17)");
	System.out.println(r.getJs());
	System.out.println(r.getPerms());
    }
}