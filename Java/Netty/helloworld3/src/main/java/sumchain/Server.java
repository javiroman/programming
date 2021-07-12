package sumchain;

import io.netty.channel.ChannelFuture;
import io.netty.channel.EventLoopGroup;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import java.net.InetSocketAddress;

public class Server {
    private final int port;

    public Server(int port) {
        this.port = port;
    }

    public static void main(String[] args) throws Exception {
        if (args.length != 1) {
            System.err.println("Usage: " + Server.class.getSimpleName() +
                    " <port>"
            );
            return;
        }
        int port = Integer.parseInt(args[0]);
        new Server(port).start();
    }

    public void start() throws Exception {
        // ChannelHandler: Data received from the client, the server business logic
        final ServerHandler serverHandler = new ServerHandler();

        EventLoopGroup group = new NioEventLoopGroup();
        try {
            // Bootstrapping: Startup code that configures the server
            ServerBootstrap b = new ServerBootstrap();

            /*
              1. Bind to the port on which the server will listen for and accept
              incoming connection requests.

              2. Configure Channels to notify an EchoServerHandler instance about inbound
              messages.
             */
            b.group(group)
                    .channel(NioServerSocketChannel.class) // Specifies the use of an NIO transport channel.
                    .localAddress(new InetSocketAddress(port)) // Sets the socket address.
                    .childHandler(new ChannelInitializer<SocketChannel>() {
                        /*
                          Adds an EchoServerHandler to the Channel's ChannelPipeline
                         */
                        @Override
                        public void initChannel(SocketChannel ch) throws Exception {
                            System.out.println("Registering Netty Pipeline");
                            ch.pipeline().addLast(serverHandler);
                        }
                    });

            // Binds the server asynchronously; sync() waits for the bind to complete.
            ChannelFuture f = b.bind().sync();

            System.out.println(Server.class.getName() +
                    " started and listening for connections on " + f.channel().localAddress());

            // Gets the CloseFuture of the Channel and blocks the current thread until it's complete.
            f.channel().closeFuture().sync();
        } finally {
            // Shuts down the EventLoopGroup releasing all resources.
            group.shutdownGracefully().sync();
        }
    }
}