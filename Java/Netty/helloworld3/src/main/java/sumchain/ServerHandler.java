package sumchain;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.util.CharsetUtil;

public class ServerHandler extends ChannelInboundHandlerAdapter {

    public ServerHandler() {
        System.out.println("ServerHandler CONSTRUCTOR");
    }


    /*
      Called for each incoming message.
     */
    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        System.out.println("ServerHandler READ");

        // Stores the received client message.
        ByteBuf in = (ByteBuf) msg;

        System.out.println("Server received: " + in.toString(CharsetUtil.UTF_8));

        // Writes the received message to the sender without flushing the outbound messages.
        ctx.write(in);
    }



    /*
      Notifies the handler that the last call made to channel-
      Read() was the last message in the current batch.
     */
    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) throws Exception {
        System.out.println("Flushing pending messages to the remote peer and channel closing");
        ctx.writeAndFlush(Unpooled.EMPTY_BUFFER)
                .addListener(ChannelFutureListener.CLOSE);
    }

    /*
      Called if an exception is thrown during the read operation.
     */
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        cause.printStackTrace();
        ctx.close();
    }
}