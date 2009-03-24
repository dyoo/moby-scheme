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
import org.w3c.dom.NodeList;


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
	    return parseNode(topElement);
	} catch (Exception e) {
	    return Empty.EMPTY;
	}
    }


    private List parseNode(Node n) {
	switch (n.getNodeType()) {
	case Node.ATTRIBUTE_NODE:
	case Node.CDATA_SECTION_NODE:
	case Node.COMMENT_NODE:
	case Node.DOCUMENT_FRAGMENT_NODE:
	case Node.DOCUMENT_NODE:
	case Node.DOCUMENT_TYPE_NODE:
	case Node.ELEMENT_NODE:
	    return parseElement((Element) n);
	case Node.ENTITY_NODE:
	case Node.ENTITY_REFERENCE_NODE:
	case Node.NOTATION_NODE:
	case Node.PROCESSING_INSTRUCTION_NODE:
	case Node.TEXT_NODE:
	default:
	    throw new RuntimeException("Impossible");
	}
    }

    private List parseElement(Element e) {
	String tagName = e.getTagName();
	NodeList children = e.getChildNodes();

	List parsedChildren = Empty.EMPTY;
	parsedChildren = new Pair(tagName, parsedChildren);
	// Fixme: add attributes
	for(int i = 0; i < children.getLength(); i++) {
	    parsedChildren = new Pair(parseNode(children.item(i)),
				      parsedChildren);
	}
	return Kernel.reverse(parsedChildren);
    }
}