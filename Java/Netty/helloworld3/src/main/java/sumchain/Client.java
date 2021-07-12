package sumchain;

import io.netty.bootstrap.Bootstrap;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;

import java.util.logging.Logger;

public class Client {

    public static void main(String[] args) {
        NioEventLoopGroup workerGroup = new NioEventLoopGroup();
        Bootstrap b = new Bootstrap();
        b.group(workerGroup)
                .channel(NioSocketChannel.class)
                .handler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    public void initChannel(SocketChannel ch) throws Exception {
                        ch.pipeline()
                                .addLast(new ClientHandlerInFirst()) // In 1
                                .addLast(new ClientHandlerInSecond()) // In 2
                                .addLast(new ClientHandlerOutSecond()) // Out 2
                                .addLast(new ClientHandlerOutFirst()) // Out 1
                                .addLast(new ClientHandler()); // Head and
                    }
                });

        String serverIp = "localhost";
        System.out.println("Connecting to server address -> localhost:8888");

        b.connect(serverIp, 8888);
    }
}
