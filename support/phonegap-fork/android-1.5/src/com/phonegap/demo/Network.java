package com.phonegap.demo;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.net.URLConnection;



public class Network {
    public String getUrl(String urlString) {
	try {
	    URL url = new URL(urlString);
	    URLConnection conn = url.openConnection();
	    BufferedReader in = new BufferedReader
		(new InputStreamReader(conn.getInputStream()));
	    StringWriter w = new StringWriter();
	    BufferedWriter writer = new BufferedWriter(w);
	    copyReaderToWriter(in, writer);
	    in.close();
	    writer.close();
	    return w.getBuffer().toString();
	} catch (Throwable e) {
	    e.printStackTrace();
	    return new String("");
	}	
    }

    
    private void copyReaderToWriter(Reader r, Writer w) throws IOException {
        int b;
        while (true) {
	    b = r.read();
	    if (b == -1) {
		break;
	    }
	    w.write(b);
        }
    }

}