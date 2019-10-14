public final class Util {
    private Util() {}

    public static java.io.PrintWriter getPrintWriter(String filename) {
        return getPrintWriter(filename,false);
    }

    public static java.io.PrintWriter getPrintWriter(String filename, 
                                                     boolean append) {
        try {
            java.io.File file = new java.io.File(filename);
            java.io.PrintWriter pw = new java.io.PrintWriter
                (new java.io.OutputStreamWriter
                 (new java.io.BufferedOutputStream
                  (new java.io.FileOutputStream(file,append))), true);
            return pw;
        } catch ( Exception e ) {
            throw new IllegalStateException(e);
        }
    }

}