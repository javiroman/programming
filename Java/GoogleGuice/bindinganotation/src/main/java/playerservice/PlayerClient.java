package playerservice;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Module;

public class PlayerClient {

    public static void main(String[] args) {

        PlayerModule module = new PlayerModule();
        Injector injector = Guice.createInjector(new Module[]{module});

        @Good Player player = (Player)injector.getInstance(Player.class);
        player.bat();
        player.bowl();
    }
}
