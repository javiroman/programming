package playerservice;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.Module;
import com.google.inject.name.Named;
import com.google.inject.name.Names;

public class PlayerClient {

    public static void main(String[] args) {

        Injector injector = Guice.createInjector(new PlayerModule());

        Player goodPlayer = (Player)injector.getInstance(Key.get(Player.class, Names.named("Good")));
        Player badPlayer = (Player)injector.getInstance(Key.get(Player.class, Names.named("Bad")));

        goodPlayer.bat();
        badPlayer.bowl();
    }
}
