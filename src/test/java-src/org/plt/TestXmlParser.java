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
	assertEquals(new Pair("hello",
			      new Pair(Empty.EMPTY,
				       Empty.EMPTY)),
		     parser.parseString("<hello/>"));
    }

    @Test public void testParseWithAttributes() {
	List attr = new Pair("foo",
			     new Pair("bar",
				      Empty.EMPTY));
	assertEquals(new Pair("world",
			      new Pair(new Pair(attr, Empty.EMPTY),
				       Empty.EMPTY)),
		     parser.parseString("<world foo=\"bar\"/>"));
    }

}
