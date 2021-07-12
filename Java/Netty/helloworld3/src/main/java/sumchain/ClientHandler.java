package sumchain;

import io.netty.channel.ChannelHandlerContext;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandler.Sharable;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.util.CharsetUtil;

/*
  Marks this class as one whose instances can be shared among channels.
 */
@Sharable
public class ClientHandler extends SimpleChannelInboundHandler<ByteBuf> {

    public ClientHandler() {
        System.out.println("5 ClientHandler Constructor");
    }

    /*
     When notified that the channel is active, sends a message. A channel is active
     when a connection has been established, so the method is invoked when the connections
     is established.
     */
    @Override
    public void channelActive(ChannelHandlerContext ctx) {
        String cad = "Netty rocks";

        System.out.println("ClientHandler Channel ACTIVE");
        System.out.println("ClientHandler sending message: " + cad);
        ctx.writeAndFlush(Unpooled.copiedBuffer(cad, CharsetUtil.UTF_8));
    }

    /*
     Logs a dump of the received message form the server. This method is called whenever
     data is received. Note that the message sent by the server may be received in chunks.
     That is, if the server sends 5 bytes, there’s no guarantee that all 5 bytes will be received
     at once. Even for such a small amount of data, the channelRead0() method could be
     called twice, first with a ByteBuf (Netty’s byte container) holding 3 bytes, and second
     with a ByteBuf holding 2 bytes. As a stream-oriented protocol, TCP guarantees that
     the bytes will be received in the order in which they were sent by the server.
     */
    @Override
    public void channelRead0(ChannelHandlerContext ctx, ByteBuf in) {
        System.out.println("Client received from server: " + in.toString(CharsetUtil.UTF_8));
    }

    /*
     On exception, logs the error and close the channel.
     */
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        cause.printStackTrace();
        ctx.close();
    }
}
