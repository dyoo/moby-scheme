/*
 * Minimal PNG encoder to create MIDP images from RGBA arrays.
 * 
 * Copyright 2006 Christian Froeschlin 
 *
 * Changelog:
 *
 * 09/22/08: Fixed Adler checksum calculation and byte order
 *           for storing length of zlib deflate block. Thanks
 *           to Miloslav R?ži?ka for noting this.
 *
 * www.chrfr.de
 *
 */

package org.plt.gui;

import java.io.*;
import javax.microedition.lcdui.Image;

public class PNGEncoder
{
  
  public static Image toImage(int width, int height, byte[] alpha, byte[] red, byte[] green, byte[] blue)
  {
    try    
    {
      byte[] png = toPNG(width, height, alpha, red, green, blue);
      return Image.createImage(png, 0, png.length);
    }
    catch (IOException e)
    {
      return null;
    }
  }
  
  public static byte[] toPNG(int width, int height, byte[] alpha, byte[] red, byte[] green, byte[] blue) throws IOException
  {
    byte[] signature = new byte[] {(byte) 137, (byte) 80, (byte) 78, (byte) 71, (byte) 13, (byte) 10, (byte) 26, (byte) 10};
    byte[] header = createHeaderChunk(width, height);
    byte[] data = createDataChunk(width, height, alpha, red, green, blue);
    byte[] trailer = createTrailerChunk();
    
    ByteArrayOutputStream png = new ByteArrayOutputStream(signature.length + header.length + data.length + trailer.length);
    png.write(signature);
    png.write(header);
    png.write(data);
    png.write(trailer);
    return png.toByteArray();
  }

  public static byte[] createHeaderChunk(int width, int height) throws IOException
  {
    ByteArrayOutputStream baos = new ByteArrayOutputStream(13);
    DataOutputStream chunk = new DataOutputStream(baos);
    chunk.writeInt(width);
    chunk.writeInt(height);
    chunk.writeByte(8); // Bitdepth
    chunk.writeByte(6); // Colortype ARGB
    chunk.writeByte(0); // Compression
    chunk.writeByte(0); // Filter
    chunk.writeByte(0); // Interlace    
    return toChunk("IHDR", baos.toByteArray());
  }

  public static byte[] createDataChunk(int width, int height, byte[] alpha, byte[] red, byte[] green, byte[] blue) throws IOException
  {
    int source = 0;
    int dest = 0;
    byte[] raw = new byte[4*(width*height) + height];
    for (int y = 0; y < height; y++)
    {
      raw[dest++] = 0; // No filter
      for (int x = 0; x < width; x++)
      {
        raw[dest++] = red[source];
        raw[dest++] = green[source];
        raw[dest++] = blue[source];
        raw[dest++] = alpha[source++];
      }
    }
    return toChunk("IDAT", toZLIB(raw));
  }

  public static byte[] createTrailerChunk() throws IOException
  {
    return toChunk("IEND", new byte[] {});
  }
  
  public static byte[] toChunk(String id, byte[] raw) throws IOException
  {
    ByteArrayOutputStream baos = new ByteArrayOutputStream(raw.length + 12);
    DataOutputStream chunk = new DataOutputStream(baos);
    
    chunk.writeInt(raw.length);
    
    byte[] bid = new byte[4];
    for (int i = 0; i < 4; i++)
    {
      bid[i] = (byte) id.charAt(i);
    }
    
    chunk.write(bid);
        
    chunk.write(raw);
    
    int crc = 0xFFFFFFFF;
    crc = updateCRC(crc, bid);  
    crc = updateCRC(crc, raw);    
    chunk.writeInt(~crc);
    
    return baos.toByteArray();
  }

  static int[] crcTable = null;
  
  public static void createCRCTable()
  {
    crcTable = new int[256];
    
    for (int i = 0; i < 256; i++)
    {
      int c = i;
      for (int k = 0; k < 8; k++)
      {
        c = ((c & 1) > 0) ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
      }
      crcTable[i] = c;
    }
  }
  
  public static int updateCRC(int crc, byte[] raw)
  {
    if (crcTable == null)
    {
      createCRCTable();
    }
    
    for (int i = 0; i < raw.length; i++)
    {
      crc = crcTable[(crc ^ raw[i]) & 0xFF] ^ (crc >>> 8);      
    }
    
    return crc;
  }


  // Creates a single zlib block contain a single
  // uncompressed deflate block. Must be < 64K!
  public static byte[] toZLIB(byte[] raw) throws IOException
  {    
    byte tmp;
    ByteArrayOutputStream baos = new ByteArrayOutputStream(raw.length + 5 + 6);
    DataOutputStream zlib = new DataOutputStream(baos);
    tmp = (byte) (8 + (7 << 16));
    zlib.writeByte(tmp);      // Compression + Flags
    zlib.writeByte((31 - ((tmp << 8) % 31)) % 31); // FCHECK (compr/dict 0)
    
    //Uncompressed deflate block
    zlib.writeByte((byte) 1);    //Final flag set, Compression type 0
    char length = (char) raw.length;
    zlib.writeByte((byte)(length & 0xFF));           //Length LSB
    zlib.writeByte((byte)((length & 0xFF00) >> 8));  //Length MSB
    zlib.writeByte((byte)(~length & 0xFF));          //Length 1st complement LSB
    zlib.writeByte((byte)((~length & 0xFF00) >> 8)); //Length 1st complement MSB 
    zlib.write(raw);         //Data
    
    // zlib block check sum
    zlib.writeInt(calcADLER32(raw));
    return baos.toByteArray();
  }

  // Unverified (the Java PNG loader did not complain at any value)
  public static int calcADLER32(byte[] raw)
  {
    int s1 = 1;
    int s2 = 0;
    for (int i = 0; i < raw.length; i++)
    {
      int abs = raw[i] >=0 ? raw[i] : (raw[i] + 256);
      s1 = (s1 + abs) % 65521;
      s2 = (s2 + s1) % 65521;      
    }
    return (s2 << 16) + s1;
  }
}
