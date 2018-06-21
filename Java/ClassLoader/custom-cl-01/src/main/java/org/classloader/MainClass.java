package org.classloader;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class MainClass {
    public static void main(String[] args) {
        // This will instantiate the object from the allready loaded class from the System Classloader
        com.you.test.YourClassCausingJarHell yourClass = new com.you.test.YourClassCausingJarHell();

        String[] pathsToJars = {"jar_path1", "jar_path2"};
        ClassLoader loader = new ParentLastClassLoader(Thread.currentThread().getContextClassLoader(),
                pathsToJars);

        Class correctClass = null;
        try {
            correctClass = loader.loadClass("com.you.test.YourClassCausingJarHell");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }

        Method theMethod = null;
        try {
            theMethod = correctClass.getMethod("theMethodYouWant");
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }

        // This calls the right method from the right class.
        try {
            theMethod.invoke(correctClass.getConstructor().newInstance());
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }

    }
}