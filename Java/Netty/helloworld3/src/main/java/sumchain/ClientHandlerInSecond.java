package sumchain;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;

public class ClientHandlerInSecond extends ChannelInboundHandlerAdapter {

    public ClientHandlerInSecond() {
        System.out.println("2 ClientHandlerInSecond Constructor");
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        System.out.println("ClientHandlerInSecond channelRead");
        //LoopBackTimeStamp ts = (LoopBackTimeStamp) msg;
        //ctx.writeAndFlush(ts); //recieved message sent back directly
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        // Close the connection when an exception is raised.
        cause.printStackTrace();
        ctx.close();
    }
}