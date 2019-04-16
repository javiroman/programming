import java.io.IOException;

public class Main1 {

    public static void main(String[] args) {
        System.out.println("Hello World!");

        Process process = null;
        try {
            process =
                    Runtime.getRuntime().exec(
                            new String[]{"sh", "-c", "cat /etc/services"});
        } catch (IOException e) {
            e.printStackTrace();
        }

        String stdout = new String("");
        String stderr = new String("");
        int exitValue = -1;

        try {
            assert process != null;
            int c;
            while ((c = process.getErrorStream().read()) != -1) {
                stderr += (char) c;
            }

            while ((c = process.getInputStream().read()) != -1) {
                stdout += (char) c;
            }

            exitValue = process.waitFor();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println(stderr);
        System.out.println(stdout);
        System.out.println(exitValue);
    }
}
