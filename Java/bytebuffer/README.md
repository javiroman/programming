# java.nio.ByteBuffer

https://docs.oracle.com/javase/8/docs/api/java/nio/ByteBuffer.html

http://www.kdgregory.com/index.php?page=java.byteBuffer

https://worldmodscode.wordpress.com/2012/12/14/the-java-bytebuffer-a-crash-course/


Java (New I/O), and it is one of the more interesting packages because it 
supports a channel-based approach to I/O operations. The NIO classes
are contained in the five packages shown here:

1. java.nio: Top-level package for the NIO system. Encapsulates various types
of buffers that contain data operated upon by the NIO system.

2. java.nio.channels: Supports channels, which are essentially open I/O
connections. Are Channels of various types, which represent connections 
to entities capable of performing I/O operations.

3. java.nio.channels.spi: Supports service providers for channels.

4. java.nio.charset: Encapsulates character sets. Also supports encoders and
decoders that convert characters to bytes and bytes to characters, respectively.

5. java.nio.charset.spi: Supports service providers for character sets.



## Introduction

it is important to emphasize that the NIO subsystem is not intended to
replace the I/O classes found in java.io. Instead, the NIO classes supplement 
the standard I/O system, giving you an alternative approach,
which can be beneficial in some circumstances.

The NIO system is built on two foundational items: buffers and channels. A
buffer holds data. A channel represents an open connection to an I/O device, 
such as a file or a socket. In general, to use the NIO system, you obtain 
a channel to an I/O device and a buffer to hold data. You then operate on 
the buffer, inputting or outputting data as needed.

The entities used by NIO are:

- Buffers

Buffers are defined in the java.nio package. All buffers are subclasses of the
Buffer class, which defines the core functionality common to all buffers: 
current position, limit, and capacity. The current position is the index within 
the buffer at which the next read or write operation will take place. 
The current position is advanced by most read or write operations.  The limit 
is the index of the end of the buffer. The capacity is the number of elements 
that the buffer can hold. Buffer also supports mark and reset, among other 
methods.

- Channels

Channels are defined in java.nio.channels. A channel represents an open
connection to an I/O source or destination. You obtain a channel by calling 
getChannel(  ) on an object that supports channels.

- Charsets

A charset defines the way that bytes are mapped to characters. You can encode 
a sequence of characters into bytes using an encoder. You can decode a 
sequence of bytes into characters using a decoder.  Charsets, encoders, 
and decoders are supported by classes defined in the java.nio.charset package.
Because default encoders and decoders are provided, you will not often need 
to work explicitly with charsets.

- Selectors

A selector supports key-based, non-blocking, multiplexed I/O. In other words,
selectors enable you to perform I/O through multiple channels. Selectors are 
supported by classes defined in the java.nio.channels package. Selectors are 
most applicable to socket-backed channels.


ByteBuffer API has been in Java since 1.4. The name itself suggests that these
contain bytes of data. The data to be stored in the buffer can be Integer (int,
long), Characters (char), Floating value (double), all these are converted into
bytes before being stored in the buffer array. 

A ByteBuffer is a buffer which provides for transferring bytes from a source to
a destination. In addition to storage like a buffer array, it also provides
abstractions such as current position, limit, capacity, etc. A FileChannel is
used for transferring data to and from a file to a ByteBuffer. ByteBuffer also
provides methods to directly store and restore the bytes of primitive types
such as integer, short, long, etc.

This buffer abstraction is an evolution of using plain byte[] arrays.


ByteBuffer can be of two types:

1. Direct ByteBuffer
2. Non-Direct ByteBuffer

Given a direct byte buffer, the Java virtual machine will make a best effort to
perform native I/O operations directly upon it. That is, it will attempt to
avoid copying the buffer's content to (or from) an intermediate buffer before
(or after) each invocation of one of the underlying operating system's native
I/O operations.

- Creating a ByteBuffer

To begin with, we need a ByteBuffer to read and write data into. Create a
ByteBuffer as follows:

```java
ByteBuffer buf = ByteBuffer.allocate(2048);
```

You could also initialize a ByteBuffer from an array of bytes. The ByteBuffer
will be backed by the array and provide facilities such as current position,
etc.

```java
byte[] byteArray = new byte[2048];
ByteBuffer buf = ByteBuffer.wrap(byteArray);
```

- Reading and Writing into a ByteBuffer

Once the ByteBuffer is allocated, you can read bytes from a file as follows.

```java
FileInputStream in = new FileInputStream(inputFile);
int len = in.getChannel().read(buf);
```

To write out the data from the ByteBuffer into a file, do the following:

```java
FileOutputStream out = new FileOutputStream(outputFile);
out.getChannel().write(buf);
```

- Copying Files Using a ByteBuffer

With the plumbing out of the way, here is the loop to copy data from one file
to another.

```java
while( in.getChannel().read(buf) != -1  ) {
    buf.flip();
    out.getChannel().write(buf);
    buf.clear();

}
```

Reading from the InputStream channel returns -1 on hitting EOF (end-of-file).
At that point flip the buffer. This sets the position of the pointer within the
buffer to 0 and the limit to the last byte read. Following which, bytes can be
transferred from the pointer position to the OutputStream channel till the
limit. At which point, the buffer can be cleared to reset the position and the
limit.

- Other Ways of Writing to ByteByffer

Of course, reading bytes from a file channel is not the only way to write into
a ByteBuffer. We have the following methods for writing data of various types
into the ByteBuffer. These methods write the binary representation of the data
into the ByteBuffer: putChar, putDouble, putFloat, putInt, putLong, putShort.

These methods write the byte values of the appropriate types into the
ByteBuffer. For instance, putChar() puts the two bytes of a char into the
ByteBuffer without encoding it with a character set. These methods can be used
when a strict binary representation of the data needs to be stored and/or
transported (for example across a socket).

- Convert Integer to Byte Array

ByteBuffer is useful for binary conversion of data to and from Java primitive
data. For instance suppose you want to convert an integer to an array of bytes.
The following shows an integer and the hex value of the bytes.


```java
int value = 389745347;
System.out.printf("%1$d => 0x%1$X", value);

//prints:
389745347 => 0x173B0AC3
```

To convert the integer to a byte array, you can use the ByteBuffer as folows.
Allocate a ByteBuffer, write the integer value into it using putInt() and get
the byte array. As simple as that.


```java
ByteBuffer buf = ByteBuffer.allocate(10);
buf.putInt(value);
buf.flip();
byte[] arr = buf.array();
System.out.print(" { ");
for (int i = 0 ; i < buf.limit() ; i++)
    System.out.printf("0x%X, ", arr[i]);
System.out.println("}");
 
// prints:
{ 0x17, 0x3B, 0xA, 0xC3,  }" }")
```

- BigInteger to Byte Array

Storing and restoring a BigInteger is a bit involved because the ByteBuffer
does not provide methods to directly read or write a BigInteger from a
ByteBuffer. In the next section, we show how to do that.

The BigInteger is an arbitrary precision integer provided in Java. Most math
operations including addition, subtraction, etc are provided by the BigInteger
class. One way of storing and transmitting a BigInteger is to convert it to a
byte array and use a ByteBuffer. Let’s see how to do that.

Convert a BigInteger to a byte array, show the byte representation and store it
in a file.


```java
BigInteger bint = new BigInteger("23784328748372478");
byte[] arr = bint.toByteArray();
System.out.printf("%1$s => { ", bint.toString());
for (int i = 0 ; i < arr.length ; i++)
    System.out.printf("0x%X, ", arr[i]);
System.out.println("}");
FileOutputStream out = new FileOutputStream("bignum");
ByteBuffer buf = ByteBuffer.allocate(25);
buf.put(arr);
buf.flip();
out.getChannel().write(buf);
out.close();
 
// prints:
23784328748372478 => { 0x54, 0x7F, 0xB8, 0x92, 0x44, 0x91, 0xFE,  }
 
// show contents of file: od -t x1 bignum
0000000 54 7f b8 92 44 91 fe
0000007 
```

Now let us read the file, load the bytes into a ByteBuffer and convert to a
BigInteger:


```java
FileInputStream in = new FileInputStream("bignum");
ByteBuffer buf = ByteBuffer.allocate(25);
in.getChannel().read(buf);
buf.flip();
byte[] arr = Arrays.copyOf(buf.array(), buf.limit());
BigInteger bint = new BigInteger(arr);
System.out.printf("%1$s => { ", bint.toString());
for (int i = 0 ; i < arr.length ; i++)
    System.out.printf("0x%X, ", arr[i]);
System.out.println("}");
 
// prints:
23784328748372478 => { 0x54, 0x7F, 0xB8, 0x92, 0x44, 0x91, 0xFE,  }
```
