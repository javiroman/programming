package add.service;

import com.google.inject.Binder;
import com.google.inject.Module;

public class AddModule implements Module{

    public void configure(Binder binder) {
        binder.bind(Add.class).to(SimpleAdd.class);
    }

}
