package org.plt.lib;

import org.plt.types.*;
import org.plt.parser.XmlParser;
import org.plt.Kernel;

public class Parser {
    public static Object parseXml(Object content) {
	XmlParser p = new XmlParser();
	return p.parseString(content.toString());
    }

    public static Object splitWhitespace(Object content) {
	String[] vals = content.toString().split("[ \r\n\t]+");
	List l = Empty.EMPTY;
	for(int i = 0; i < vals.length; i++) {
	    if (vals[i].length() > 0)
		l = new Pair(vals[i], l);
	}
	return Kernel.reverse(l);
    }
}
