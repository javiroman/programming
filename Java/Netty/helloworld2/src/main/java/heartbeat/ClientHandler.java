package heartbeat;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;

import java.util.logging.Logger;

public class ClientHandler extends ChannelInboundHandlerAdapter {

    private final static Logger LOGGER = Logger.getLogger(NettyClient.class.getName());

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        LOGGER.info("InboudHandler read");
        LoopBackTimeStamp ts = (LoopBackTimeStamp) msg;
        ctx.writeAndFlush(ts); //recieved message sent back directly
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        // Close the connection when an exception is raised.
        cause.printStackTrace();
        ctx.close();
    }
}