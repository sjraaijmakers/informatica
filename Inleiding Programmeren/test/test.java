public class test {
    public static void main (String[] args){
        try {
            methode1();
            methode2();
        }
        catch(Exception e){
            System.out.println("caught Exception");
        }
        System.out.println("outside");
    }

    static void methode1() throws Exception{
        try {
            if(false){
                throw new Exception("exception in methode 1");
            }
            System.out.println("methode 1 again");
        }
        finally {
            System.out.println("methode 1 finally");
        }
    }

    static void methode2() throws Exception{
        try {
            if(false){
                throw new Exception("exception in methode 2");
            }
            System.out.println("methode 2 again");
        }
        finally {
            System.out.println("methode 2 finally");
        }
    }
}
