/*
  Reference:
  https://docs.oracle.com/javase/8/docs/technotes/guides/versioning/spec/versioning2.html#wp90779
 */
package org.classloader;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

public class MainClass {
    public static void main(String[] args) {

        Map<String, String> inputMap = new HashMap<String, String>();
        inputMap.put("name", "Java2Novice");
        inputMap.put("site", "http://java2novice.com");

        // convert map to JSON String
        // notice that we enabled pretty printing in the below line
        Gson gsonObj = new GsonBuilder().setPrettyPrinting().create();
        String jsonStr = gsonObj.toJson(inputMap);
        System.out.println(jsonStr);

        Package pkg = Package.getPackage("com.google.gson");
        //Package pkg = com.google.gson.Gson.class.getPackage();
        System.out.println("Package name:\t" + pkg.getName());
        System.out.println("Spec title:\t" + pkg.getSpecificationTitle());
        System.out.println("Spec vendor:\t" + pkg.getSpecificationVendor());
        System.out.println("Spec version:\t" + pkg.getSpecificationVersion());
        System.out.println("Impl title:\t" + pkg.getImplementationTitle());
        System.out.println("Impl vendor:\t" + pkg.getImplementationVendor());
        System.out.println("Impl version:\t" + pkg.getImplementationVersion());

        String[] pathsToJars = {"new-jar/gson-2.8.5.jar"};
        ClassLoader loader = new ParentLastClassLoader(Thread.currentThread().getContextClassLoader(),
                pathsToJars);

        Class correctClass = null;
        try {
            correctClass = loader.loadClass("com.google.gson.GsonBuilder");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }

        Method theMethod = null;
        try {
            theMethod = correctClass.getMethod("create");
            System.out.println("method = " + theMethod.toString());
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }

        System.out.println(correctClass.getClass().getPackage().getSpecificationVersion());
        System.out.println(correctClass.getClass().getPackage().getImplementationVersion());
        System.out.println(correctClass.getClass().getPackage().getSpecificationTitle());

        // This calls the right method from the right class.
        try {
            Object a = theMethod.invoke(correctClass.getConstructor().newInstance());
//            a.toJson();
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
