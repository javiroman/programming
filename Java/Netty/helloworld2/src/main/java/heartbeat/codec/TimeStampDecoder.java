package heartbeat.codec;

import heartbeat.LoopBackTimeStamp;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.ByteToMessageDecoder;

import java.util.List;
import java.util.logging.Logger;

public class TimeStampDecoder extends ByteToMessageDecoder {

    private final static Logger LOGGER = Logger.getLogger(TimeStampDecoder.class.getName());

    @Override
    protected void decode(ChannelHandlerContext ctx,
                          ByteBuf in,
                          List<Object> out) throws Exception {

        LOGGER.info("Decoder working");
        final int messageLength = Long.SIZE/Byte.SIZE *2;
        if (in.readableBytes() < messageLength) {
            return;
        }

        byte [] ba = new byte[messageLength];
        in.readBytes(ba, 0, messageLength);  // block until read 16 bytes from sockets
        LoopBackTimeStamp loopBackTimeStamp = new LoopBackTimeStamp();
        loopBackTimeStamp.fromByteArray(ba);
        out.add(loopBackTimeStamp);
    }
}