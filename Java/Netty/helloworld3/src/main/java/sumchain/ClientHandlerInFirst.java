package sumchain;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.util.CharsetUtil;

import javax.jws.Oneway;

public class ClientHandlerInFirst extends ChannelInboundHandlerAdapter {

    public ClientHandlerInFirst() {
        System.out.println("1 ClientHandlerInFirst Constructor");
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        System.out.println("ClientHandlerInFirst channelRead");
        //LoopBackTimeStamp ts = (LoopBackTimeStamp) msg;
        //ctx.writeAndFlush(ts); //recieved message sent back directly
        String data = ((ByteBuf) msg).toString(CharsetUtil.UTF_8);
        System.out.println("ClientHandlerInFirst.channelRead Data received:" + data);
        ctx.fireChannelRead(Unpooled.copiedBuffer("[EchoInboundHandler1] " + data, CharsetUtil.UTF_8));
    }



    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        // Close the connection when an exception is raised.
        cause.printStackTrace();
        ctx.close();
    }
}