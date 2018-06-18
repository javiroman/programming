/*
   This is the Module class which makes use of Guice API
   to establish Bindings in an Application. This is a Module
   can be configured with some of Bindings which is done
   through the Binder class. In Guice terms, a Binding
   refers to the process of providing association to an
   Interface to its original Implementation.

   In this code, we are telling Guice to bind the implementation
   for Add interface to SimpleAdd class which literally tells
   that calls on Add.add() made by the clients will be re-directed
   to SimpleAdd.add().
 */
package add.service;

import com.google.inject.Binder;
import com.google.inject.Module;

public class AddModule implements Module{
    public void configure(Binder binder) {
        binder.bind(Add.class).to(SimpleAdd.class);
    }
}
