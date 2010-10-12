package org.plt;

import org.plt.types.*;
import org.plt.Kernel;
import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import org.plt.parser.XmlParser;

public class TestXmlParser {
    XmlParser parser;

    @Before
    public void setup() {
	parser = new XmlParser();
    }

    @Test public void testParse() {
	assertEquals(new Pair(Symbol.makeInstance("hello"),
			      new Pair(Empty.EMPTY,
				       Empty.EMPTY)),
		     parser.parseString("<hello/>"));
    }

    @Test public void testParseWithAttributes() {
	List attr = new Pair(Symbol.makeInstance("foo"),
			     new Pair("bar",
				      Empty.EMPTY));
	assertEquals(new Pair(Symbol.makeInstance("world"),
			      new Pair(new Pair(attr, Empty.EMPTY),
				       Empty.EMPTY)),
		     parser.parseString("<world foo=\"bar\"/>"));
    }

    @Test public void testNestedChildren() {
	List worldList = new Pair(Symbol.makeInstance("world"),
				  new Pair(Empty.EMPTY,
					   Empty.EMPTY));
	assertEquals(new Pair(Symbol.makeInstance("hello"),
			      new Pair(Empty.EMPTY,
				       new Pair(worldList,
						Empty.EMPTY))),
		     parser.parseString("<hello><world/></hello>"));
    }


    @Test public void testText() {
	assertEquals(new Pair(Symbol.makeInstance("p"),
			      new Pair(Empty.EMPTY,
				       new Pair("this is a test",
						Empty.EMPTY))),
		     parser.parseString("<p>this is a test</p>"));
    }
}
