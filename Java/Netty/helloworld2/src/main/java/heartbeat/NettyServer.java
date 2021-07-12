package heartbeat;

import heartbeat.codec.TimeStampDecoder;
import heartbeat.codec.TimeStampEncoder;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.timeout.IdleStateHandler;
import io.netty.util.concurrent.DefaultEventExecutorGroup;
import io.netty.util.concurrent.EventExecutorGroup;

import java.io.IOException;

public class NettyServer {

    public static void main(String[] args) throws IOException, InterruptedException {
        NioEventLoopGroup boosGroup = new NioEventLoopGroup();
        NioEventLoopGroup workerGroup = new NioEventLoopGroup();

        ServerBootstrap bootstrap = new ServerBootstrap();
        bootstrap.group(boosGroup, workerGroup);
        bootstrap.channel(NioServerSocketChannel.class);

        // Define a separate thread pool to execute handlers with
        // slow business logic. e.g database operation
        final EventExecutorGroup group =
                new DefaultEventExecutorGroup(1500); //thread pool of 1500

        bootstrap.childHandler(new ChannelInitializer<SocketChannel>() {
            @Override
            protected void initChannel(SocketChannel ch) throws Exception {
                ChannelPipeline pipeline = ch.pipeline()
                    .addLast("idleStateHandler", // add with name
                            new IdleStateHandler(0,
                                    0,
                                    5))
                    .addLast(new TimeStampEncoder()) // add without name, name auto generated
                    .addLast(new TimeStampDecoder()) // add without name, name auto generated
                    .addLast(group,"serverHandler", new ServerHandler());
                    // run handler with slow business logic in separate thread from I/O thread
            }
        });

        bootstrap.childOption(ChannelOption.SO_KEEPALIVE, true);
        bootstrap.bind(19000).sync();
    }
}