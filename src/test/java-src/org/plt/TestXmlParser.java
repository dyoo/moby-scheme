package org.plt;

import org.plt.types.*;
import org.plt.Kernel;
import org.junit.Test;
import static org.junit.Assert.*;

import org.plt.parser.XmlParser;

public class TestXmlParser {
	@Test
	public void testParse() {
	    XmlParser parser = new XmlParser();
	    assertEquals(new Pair("hello",
				  new Pair(Empty.EMPTY,
					   Empty.EMPTY)),
			 parser.parseString("<hello/>"));
	}
}
