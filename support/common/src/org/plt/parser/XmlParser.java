package org.plt.parser;

import org.plt.types.*;
import org.plt.Kernel;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Attr;
import org.w3c.dom.Text;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.w3c.dom.CDATASection;


public class XmlParser {

    private DocumentBuilderFactory factory;
    
    public XmlParser() {
	this.factory = DocumentBuilderFactory.newInstance();
    }

    // Given string s, parses it.
    public List parseString(String s) {
	try {
	    DocumentBuilder builder = this.factory.newDocumentBuilder();
	    Element topElement = 
		builder.parse(new ByteArrayInputStream(s.getBytes())).getDocumentElement();
	    return parseElement(topElement);
	} catch (Exception e) {
	    e.printStackTrace();
	    return Empty.EMPTY;
	}
    }


    private Object parseNode(Node n) {
	switch (n.getNodeType()) {
	case Node.ATTRIBUTE_NODE:
	    return parseAttribute((Attr) n);
	case Node.CDATA_SECTION_NODE:
	    return parseCdata((CDATASection) n);
	case Node.COMMENT_NODE:
	    return null;
	case Node.DOCUMENT_FRAGMENT_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.DOCUMENT_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.DOCUMENT_TYPE_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.ELEMENT_NODE:
	    return parseElement((Element) n);
	case Node.ENTITY_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.ENTITY_REFERENCE_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.NOTATION_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.PROCESSING_INSTRUCTION_NODE:
	    throw new RuntimeException("Not handled yet");
	case Node.TEXT_NODE:
	    return parseText((Text) n);
	default:
	    throw new RuntimeException("Impossible");
	}
    }

    private String parseText(Text t) {
	return t.getData();
    }


    private String parseCdata(CDATASection c) {
	return c.getData();
    }

    private List parseAttribute(Attr a) {
	return new Pair(Symbol.makeInstance(a.getName()),
			new Pair(a.getValue(),
				 Empty.EMPTY));
	    
    }

    private List parseElement(Element e) {
	List parsed = Empty.EMPTY;

	String tagName = e.getTagName();
	parsed = new Pair(Symbol.makeInstance(tagName), parsed);


	NamedNodeMap attrs = e.getAttributes();
	if (attrs == null) {
	    parsed = new Pair(Empty.EMPTY, parsed);
	} else {
	    List attrList = Empty.EMPTY;
	    for(int i = 0; i < attrs.getLength(); i++) {
		attrList = new Pair(parseAttribute((Attr)attrs.item(i)),
				    attrList);
	    }
	    parsed = new Pair(attrList, parsed);
	}
 
	
	NodeList children = e.getChildNodes();
	for(int i = 0; i < children.getLength(); i++) {
	    Object nextChild = parseNode(children.item(i));
	    if (nextChild == null)
		continue;
	    else if (nextChild instanceof String &&
		!parsed.isEmpty() &&
		parsed.first() instanceof String) {
		// Collapse adjacent strings.
		parsed = new Pair(((String)parsed.first()) + ((String)nextChild),
				  parsed.rest());
	    } else {
		parsed = new Pair(nextChild, parsed);
	    }
	}
	return Kernel.reverse(parsed);
    }
}