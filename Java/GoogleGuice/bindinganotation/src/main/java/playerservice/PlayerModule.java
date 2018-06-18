package playerservice;

import com.google.inject.*;

public class PlayerModule implements Module{

    public void configure(Binder binder) {

        binder.bind(Player.class).annotatedWith(Good.class).to(
                GoodPlayer.class);
        binder.bind(Player.class).annotatedWith(Bad.class).to(
                BadPlayer.class);
    }
}
