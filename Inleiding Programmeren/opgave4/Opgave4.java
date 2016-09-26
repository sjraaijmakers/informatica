/*
 * DNAMatch v0.1 *
 * author    :  Dr. Quakerjack.
 * co-author :  Steven Raaijmakers
 * version   :  0.1
 * sources   :  Levenstein algorithm: http://nl.wikipedia.org/wiki/Levenshteinafstand
 *              Bubblesort algorithm: http://nl.wikipedia.org/wiki/Bubblesort
 */

import java.util.*;

public class Opgave4{

    /* Main */
    public static void main(String[] args) {
        System.out.println("Welcome to DNA Matcher v0.1");
        System.out.println();
        database = new ArrayList<String>();
        userInterface();
    }

    /* User interface */
    public static void userInterface() {
        boolean quit = false;
        do {
            System.out.print("console> ");

            String A = commandInput();
            String[] P = A.split("\\s");
            String C = P[0].toLowerCase();

            if(C.equals("help")) {
                helpuser();
            }
            else if(C.equals("quit")) {
                quit = true;
            }
            else if(C.equals("list")) {
                showList();
            }
            else if(C.equals("add") && P.length == 2) {
                addToDatabase(P[1].toUpperCase());
            }
            else if(C.equals("remove") && P.length == 2) {
                removeFromDatabase(P[1].toUpperCase());
            }
            else if(C.equals("compare") && P.length == 3) {
                compareStrings(P[1].toUpperCase(), P[2].toUpperCase());
            }
            else if(C.equals("retrieve")) {
                findInDatabase(P[1].toUpperCase());
            }
            else {
                System.out.println("Skipping...");
            }
        }
        while (quit != true);
    }

    public static ArrayList<String> database;

    /* Comand input */
    public static String commandInput() {
        Scanner input = new Scanner(System.in);
        if(input.hasNextLine()) {
            return input.nextLine();
        }
        else {
            return "";
        }
    }

    /* Print list of commanstring_input */
    public static void helpuser(){
        System.out.println("LIST OF COMMANDS");
        System.out.println("    list                       print database");
        System.out.println("    add         ...            add to database");
        System.out.println("    compare     ...     ...    compare two strings");
        System.out.println("    retrieve    ...            find in database");
        System.out.println("    remove      ...            remove from database");
        System.out.println("    quit                       stop");
        System.out.println("    help                       print this text");
        System.out.println();
    }

    /* Show list of string_input */
    public static void showList() {
        for(String s : database) {
            System.out.println(s);
        }
        System.out.println();
    }

    /* Add value to database */
    public static void addToDatabase(String i){
        database.add(i);
    }

    /* Remove value from database */
    public static void removeFromDatabase(String i){
        if(database.contains(i)){
            database.remove(database.indexOf(i));
        }
        else {
            System.out.println(i + " not in database, nothing done");
            System.out.println();
        }
    }

    /* Compare two strings */
    public static void compareStrings(String a, String aa){
        System.out.println("Difference = " + levenshtein(a, aa, true));
        System.out.println();
    }

    /* Using Levenshtein (see desc.) algorithm. */
    public static int levenshtein(String a, String b, boolean c) {
        int[][] d = new int[a.length() + 1][b.length() +1];
        for(int i = 0 ; i <= a.length(); i++){
            d[i][0] = i;
        }
        for(int j = 0; j <= b.length(); j++) {
            d[0][j] = j;
        }
        for(int i = 1; i <= a.length(); i++) {
            for(int j = 1; j <= b.length(); j++) {
                if(a.charAt(i-1) == b.charAt(j-1)) {
                    d[i][j] = d[i-1][j-1];
                }
                else {
                    d[i][j] = lowestInt(d[i-1][j] + 1, d[i][j-1] + 1, d[i-1][j-1] + 1);
                }
            }
        }
        /* Print Levenshtein diagram */
        if(c == true) {
            for(int vertical = 0; vertical <= a.length(); vertical++) {
                for(int horizontal = 0; horizontal <= b.length(); horizontal++) {
                    System.out.print(d[vertical][horizontal]+ " ");
                }
                System.out.println();
            }
            System.out.println();
        }
        return d[a.length()][b.length()];
    }

    /* Return lowest integer */
    private static int lowestInt(int a, int b, int c) {
        return Math.min(Math.min(a , b), c);
    }

    /* Find in database */
    public static void findInDatabase(String i) {
        String[] sequence = new String[database.size()];
        database.toArray(sequence);

        int r = 0;
        int[] string_input = new int[sequence.length];

        for(String s : database){
            string_input[r++] = levenshtein(i, s, false);
        }

        /* Bubblesort algorithm */
        for(int x = 0; x < sequence.length; x++) {
            for(int y = 1; y < sequence.length-x; y++) {
                    if(string_input[y - 1] > string_input[y]) {
                        int temp1 = string_input[y];
                        string_input[y] = string_input[y - 1];
                        string_input[y-1] = temp1;

                        String temp2 = sequence[y - 1];
                        sequence[y - 1] = sequence[y];
                        sequence[y] = temp2;
                    }
            }
        }

        System.out.println("Best matches:");
        for(r = 0; r < Math.min(3, sequence.length); r++){
            System.out.println(string_input[r] + "\t" + sequence[r]);
        }
        System.out.println();
    }
}
