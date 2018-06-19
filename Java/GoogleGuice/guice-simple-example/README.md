# guice-simple-example
A simple working example of Google Guice Injections

The main class, `Chemist.java`, will use Guice to instantiate all dependencies.  It is done in the following order: 

 - Module defined Eager Singletons
 - Guice injector getInstance
   - Constructor Injected parameters
   - Constructors with `@Inject`
   - Fields with `@Inject`
   - Setter methods with `@Inject`

If you don't call injector.getInstance on a class/interface, it will not get constructed.

[Guice Documentation](https://github.com/google/guice/wiki/Injections)