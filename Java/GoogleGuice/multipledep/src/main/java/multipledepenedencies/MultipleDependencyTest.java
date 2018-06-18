package multipledepenedencies;

import com.google.inject.Guice;
import com.google.inject.Injector;

public class MultipleDependencyTest {

    public static void main(String[] args) {

        Injector injector = Guice.createInjector();
        Person person = injector.getInstance(Person.class);
        person.diplayInfo();
    }
}
