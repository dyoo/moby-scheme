package org.plt.lib;

import org.plt.types.*;
import org.plt.parser.XmlParser;

public class Parser {
    public static Object parseXml(Object content) {
	XmlParser p = new XmlParser();
	return p.parseString(content.toString());
    }
}
