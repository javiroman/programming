package add.service;

import com.google.inject.Guice;
import com.google.inject.Injector;

public class AddClient {

    public static void main(String[] args) {

        Injector injector = Guice.createInjector(new AddModule());
        Add add = injector.getInstance(Add.class);
        System.out.println(add.add(10, 54));
    }
}
