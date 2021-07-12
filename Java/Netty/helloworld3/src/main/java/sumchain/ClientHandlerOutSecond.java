package sumchain;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelOutboundHandlerAdapter;
import io.netty.channel.ChannelPromise;
import io.netty.util.CharsetUtil;

public class ClientHandlerOutSecond extends ChannelOutboundHandlerAdapter {

    public ClientHandlerOutSecond(){
        System.out.println("3 ClientHandlerOutSecond Constructor");
    }

    @Override
    public void read(ChannelHandlerContext ctx) throws Exception {
        System.out.println("ClientHandlerOutSecond READ");

    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        ByteBuf in = (ByteBuf) msg;

        System.out.println("ClientHandlerOutSecond WRITE is: " + in.toString(CharsetUtil.UTF_8));

        super.write(ctx, msg, promise);
    }

    @Override
    public void flush(ChannelHandlerContext ctx) throws Exception {
        super.flush(ctx);
        System.out.println("ClientHandlerOutSecond FLUSH");
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        // Close the connection when an exception is raised.
        cause.printStackTrace();
        ctx.close();
    }
}