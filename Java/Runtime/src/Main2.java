import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class Main2 {

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

        int exitValue = -1;

        // any error message?
        StreamGobbler errorGobbler = new
                StreamGobbler(process.getErrorStream(), "ERR-> ");

        // any output?
        StreamGobbler outputGobbler = new
                StreamGobbler(process.getInputStream(), "OUT-> ");

        // kick them off
        errorGobbler.start();
        outputGobbler.start();

        try {
            exitValue = process.waitFor();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        System.out.println(exitValue);
    }

    static class StreamGobbler extends Thread
    {
        InputStream is;
        String type;

        StreamGobbler(InputStream is, String type)
        {
            this.is = is;
            this.type = type;
        }

        public void run()
        {
            try
            {
                InputStreamReader isr = new InputStreamReader(is);
                BufferedReader br = new BufferedReader(isr);
                String line=null;
                while ( (line = br.readLine()) != null)
                    System.out.println(type + line);
            } catch (IOException ioe)
            {
                ioe.printStackTrace();
            }
        }
    }
}
