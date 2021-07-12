package heartbeat.codec;

import heartbeat.LoopBackTimeStamp;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.MessageToByteEncoder;

import java.util.logging.Logger;

public class TimeStampEncoder extends MessageToByteEncoder<LoopBackTimeStamp> {

    private final static Logger LOGGER = Logger.getLogger(TimeStampEncoder.class.getName());

    @Override
    protected void encode(ChannelHandlerContext ctx,
                          LoopBackTimeStamp msg,
                          ByteBuf out) throws Exception {

        LOGGER.info("Enconder working");
        out.writeBytes(msg.toByteArray());
    }
}