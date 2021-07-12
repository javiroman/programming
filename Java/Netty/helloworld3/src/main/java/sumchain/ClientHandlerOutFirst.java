package sumchain;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelOutboundHandlerAdapter;
import io.netty.channel.ChannelPromise;
import io.netty.util.CharsetUtil;

public class ClientHandlerOutFirst extends ChannelOutboundHandlerAdapter {

    public ClientHandlerOutFirst(){
        System.out.println("4 ClientHandlerOutFirst Constructor");
    }

    @Override
    public void flush(ChannelHandlerContext ctx) throws Exception {
        super.flush(ctx);
        System.out.println("ClientHandlerOutFirst FLUSH");
    }

    @Override
    public void read(ChannelHandlerContext ctx) throws Exception {
        super.read(ctx);
        System.out.println("ClientHandlerOutFirst READ");
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        ByteBuf in = (ByteBuf) msg;
        System.out.println("ClientHandlerOutFirst was: " + in.toString(CharsetUtil.UTF_8));

        String cad = "mierda";

        System.out.println("ClientHandlerOutFirst now WRITE: " + cad);


        ctx.writeAndFlush(Unpooled.copiedBuffer(cad, CharsetUtil.UTF_8));
        super.write(ctx, msg, promise);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        // Close the connection when an exception is raised.
        cause.printStackTrace();
        ctx.close();
    }
}