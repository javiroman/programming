package javiroman.echoserver;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandler.Sharable;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.util.CharsetUtil;

/*
 Because your Echo server will respond to incoming messages, it will need to imple-
 ment interface ChannelInboundHandler , which defines methods for acting on inbound
 events. This simple application will require only a few of these methods, so it will be
 sufficient to subclass ChannelInboundHandlerAdapter , which provides a default imple-
 mentation of ChannelInboundHandler.

 @Sharable annotation: Indicates that the same instance of the annotated ChannelHandler
 can be added to one or more ChannelPipelines multiple times without a race condition.
 If this annotation is not specified, you have to create a new handler instance every
 time you add it to a pipeline because it has unshared state such as member variables.
 */
@Sharable
public class EchoServerHandler extends ChannelInboundHandlerAdapter {
    /*
      Called for each incoming message.
     */
    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
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
