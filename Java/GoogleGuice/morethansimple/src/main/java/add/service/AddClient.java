/*
  Following is the Client code which uses the Add interface.
 */
package add.service;

import com.google.inject.Guice;
import com.google.inject.Injector;

public class AddClient {
    public static void main(String[] args) {
        // We create an injector based on Module binding class.
        Injector injector = Guice.createInjector(new AddModule());

        // Get an instance of Add Class
        Add add = injector.getInstance(Add.class);

        // Get access to the interface methods
        System.out.println(add.add(10, 54));
        System.out.println(add.add2(10, 54));
    }
}
