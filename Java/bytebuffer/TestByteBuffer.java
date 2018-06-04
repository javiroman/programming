/*
 * java.nio.buffer
 *
 * A buffer is a linear, finite sequence of elements of a specific primitive
 * type. Aside from its content, the essential properties of a buffer are its
 * capacity, limit, and position:
 *
 * - A buffer's capacity is the number of elements it contains. The capacity of a
 * buffer is never negative and never changes.
 *
 * - A buffer's limit is the index of the first element that should not be read
 * or written. A buffer's limit is never negative and is never greater than its
 * capacity.
 *
 * - A buffer's position is the index of the next element to be read or written.
 * A buffer's position is never negative and is never greater than its limit.
 *
 * - A buffer's mark is the index to which its position will be reset when the
 *   reset method is invoked. The mark is not always defined, but when it is
 *   defined it is never negative and is never greater than the position. If
 *   the mark is defined then it is discarded when the position or the limit is
 *   adjusted to a value smaller than the mark. If the mark is not defined then
 *   invoking the reset method causes an InvalidMarkException to be thrown.
 *
 * The following invariant holds for the mark, position, limit, and capacity
 * values:
 *
 *      0 <= mark <= position <= limit <= capacity
 *
 */
import java.io.*;
import java.nio.*;
import java.nio.channels.*;

public class TestByteBuffer {
    public static void main(String[] args) {
        // Create a ByteBuffer using a byte array
        byte[] bytes = new byte[10];

        ByteBuffer buf = ByteBuffer.wrap(bytes);

        // Create a non-direct ByteBuffer with a 10 byte capacity
        // The underlying storage is a byte array.
        //buf = ByteBuffer.allocate(10);

        // Create a direct (memory-mapped) ByteBuffer with a 10 byte capacity.
        buf = ByteBuffer.allocateDirect(10);

        // Get the ByteBuffer's capacity: 10
        int capacity = buf.capacity(); 
        System.out.println("Buffer capacity: " + capacity);

        // Use the absolute get(). This method does not affect the position.
        // Position = 0
        // Absolute get method. Reads the byte at the given index. And puts
        // in the destination variable.
        byte b = buf.get(5);

        // java.nio.Buffer position method. 
        // Sets this buffer's position. If the mark is defined and larger than
        // the new position then it is discarded.
        buf.position(5);
        System.out.println("Setting Buffer current postion: " + 
                buf.position());

        // Relative get method. Reads the byte at this buffer's current
        // position, and then increments the position.
        b = buf.get();

        // Get the new position
        int pos = buf.position(); // 6
        System.out.println("Buffer position after get (increased in one): " + 
                pos);

        // Get remaining byte count
        // Returns the number of elements between the current position and the
        // limit.
        int rem = buf.remaining(); // 4
        System.out.println("Buffer remaining byte conunt: " + rem);

        System.out.println("Getting current Buffer limit: " + buf.limit());
        // Set the limit
        // Sets this buffer's limit. If the position is larger than the new
        // limit then it is set to the new limit. If the mark is defined and
        // larger than the new limit then it is discarded.
        buf.limit(7); // remaining=1
        System.out.println("Getting new Buffer limit: " + buf.limit());

        // This convenience method sets the position to 0
        buf.rewind(); // remaining=7 because the limit
        System.out.println("Buffer position after rewind: " + buf.position());

        // Use the absolute put(). This method does not affect the position.
        buf.put((byte)0xFF); // position=0
        System.out.println("Buffer position after put: " + buf.position());

        // Use the relative put()
        buf.put((byte)0xFF);
        System.out.println("Buffer position after put: " + buf.position());

        // This convenience method sets the position to 0
        buf.rewind(); // remaining=7
        System.out.println("Buffer position after rewind: " + buf.position());

        // Create a character ByteBuffer
        // Creates a view of this byte buffer as a char buffer.
        // The content of the new buffer will start at this buffer's current
        // position. Changes to this buffer's content will be visible in the
        // new buffer, and vice versa; the two buffers' position, limit, and
        // mark values will be independent.
        //
        // The new buffer's position will be zero, its capacity and its limit
        // will be the number of bytes remaining in this buffer divided by two,
        // and its mark will be undefined. The new buffer will be direct if,
        // and only if, this buffer is direct, and it will be read-only if, and
        // only if, this buffer is read-only.
        CharBuffer cbuf = buf.asCharBuffer();

        System.out.println("Writing string: str (3 chars)");

        // Write a string
        cbuf.put("str");
        System.out.println("CharBuffer position after put: " + cbuf.position());

        // In addition to methods for accessing the position, limit, and
        // capacity values and for marking and resetting, this class also
        // defines the following operations upon buffers:
        //
        // clear() makes a buffer ready for a new sequence of channel-read or
        // relative put operations: It sets the limit to the capacity and the
        // position to zero.
        //
        // flip() makes a buffer ready for a new sequence of channel-write or
        // relative get operations: It sets the limit to the current position
        // and then sets the position to zero.
        //
        // rewind() makes a buffer ready for re-reading the data that it
        // already contains: It leaves the limit unchanged and sets the
        // position to zero.
        cbuf.flip();

        // Convert character ByteBuffer to a string.
        // Uses characters between current position and limit so flip it first
        String s = cbuf.toString(); // str Does not affect position

        System.out.println("Reading the string: ." + s + ".");

        // Get a substring:
        // A CharSequence is a readable sequence of char values. This interface
        // provides uniform, read-only access to many different kinds of char
        // sequences. A char value represents a character in the Basic
        // Multilingual Plane (BMP) or a surrogate.
        //
        // Returns a CharSequence that is a subsequence of this sequence. The
        // subsequence starts with the char value at the specified index and
        // ends with the char value at index end - 1. The length (in chars) of
        // the returned sequence is end - start, so if start == end then an
        // empty sequence is returned.
        //
        int start = 0; // start is relative to cbuf's current position
        int end = 3;
        System.out.println("CharBuffer current position: " + cbuf.position());
        CharSequence sub = cbuf.subSequence(start, end); // str
        System.out.println("CharBuffer substring extracted: ." + sub + ".");

        // Set Byte Ordering for a ByteBuffer
        // Get default byte ordering
        // java.nio.ShortBuffer: order() 
        // Retrieves this buffer's byte order.
        // The byte order of a short buffer created by allocation or by
        // wrapping an existing short array is the native order of the
        // underlying hardware. The byte order of a short buffer created as a
        // view of a byte buffer is that of the byte buffer at the moment that
        // the view is created.
        ByteOrder order = buf.order(); // ByteOrder.BIG_ENDIAN

        String message;
        switch (order.toString()) {
            case "BIG_ENDIAN":
                message = "BIG_ENDIAN";
                break;
            case "LITTLE_ENDIAN":
                message = ".LITTLE_ENDIAN";
                break;
            default: 
                message = "Invalid order???";
                break;
        }

        System.out.println(message);

        // Put a multibyte value
        buf.putShort(0, (short)123);
        buf.get(0); // 0
        buf.get(1); // 123

        // Set to little endian
        buf.order(ByteOrder.LITTLE_ENDIAN);

        // Put a multibyte value
        buf.putShort(0, (short)123);
        buf.get(0); // 123
        buf.get(1); // 0
    }
}
