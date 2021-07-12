package heartbeat;

import heartbeat.codec.TimeStampDecoder;
import heartbeat.codec.TimeStampEncoder;
import io.netty.bootstrap.Bootstrap;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;

import java.util.logging.Logger;

public class NettyClient {

    private final static Logger LOGGER = Logger.getLogger(NettyClient.class.getName());

    public static void main(String[] args) {
        LOGGER.info("Logger Name: " + LOGGER.getName());

        NioEventLoopGroup workerGroup = new NioEventLoopGroup();
        Bootstrap b = new Bootstrap();
        b.group(workerGroup)
                .channel(NioSocketChannel.class)
                .handler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    public void initChannel(SocketChannel ch) throws Exception {
                        ch.pipeline()
                                .addLast(new TimeStampEncoder(),
                                        new TimeStampDecoder(),
                                        new ClientHandler());
                    }
        });

        String serverIp = "localhost";
        LOGGER.warning("Connecting to server address");

        b.connect(serverIp, 19000);
    }
}