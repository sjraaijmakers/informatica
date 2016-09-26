/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Programma controleert of input een palindroom is, en toont enkele waarden van de input
*/

import java.util.*;

public class Opgave2 {

    public static void main(String[] args){
        String ongefilterd = getInput();
        String gefilterd = changeInput(ongefilterd);

        System.out.println("Lengte ongefilterde zin:    " + ongefilterd.length() + " karakters");
        System.out.println("Gefilterde input:           " + gefilterd);
        System.out.println("Lengte gefilterde zin:      " + gefilterd.length() + " karakters");

        int result[] = tellen(gefilterd);
        System.out.println("Klinkers:                   " + result[0]);
        System.out.println("Aantal woorden:             " + result[1]);

        System.out.println("Palindroom?                 " + palindroom(gefilterd));
        System.out.println("-------------------------------------------------------------------------");

        frequenties(gefilterd);
    }

    /* Input controleren */
    static String getInput () {
        Scanner scan = new Scanner(System.in);
        String zin = "";
        System.out.println("Voer een zin in: ");
        if (scan.hasNextLine()){
            zin = scan.nextLine().trim();
        }
        if (zin.equals("")) {
            System.out.print("Geen invoer !");
            System.out.println("");
            System.exit (0);
        }
        return zin;
    }

    /* Input filteren */
    static String changeInput (String zin) {
    String gefilterd = "";
        for (int i = 0; i < zin.length(); i++){
            /* Hoofdlettes naar kleine letters converteren */
            if((int)zin.charAt(i) >= 65 && (int)zin.charAt(i) <= 90){
                char hoofdletter_kleineletter = (char)((int)zin.charAt(i)+32);
                gefilterd = gefilterd + hoofdletter_kleineletter;
            }
            /* Getallen toevoegen */
            else if((int)zin.charAt(i) >= 48 && (int)zin.charAt(i) <= 57) {
                gefilterd = gefilterd + zin.charAt(i);
            }
            /* Kleine letters toevoegen */
            else if((int)zin.charAt(i) >= 97 && (int)zin.charAt(i) <= 122) {
                gefilterd = gefilterd + zin.charAt(i);
            }
            /* Spaties verenkelvoudigen (en daarna toevoegen) */
            else if((int)zin.charAt(i) == 32 && (int)zin.charAt(i+1) != 32){
                gefilterd = gefilterd + zin.charAt(i);
            }
        }
    return gefilterd;
    }

    /* Tellen klinkers en aantal woorden*/
    static int[] tellen (String zin){
        int klinkers = 0;
        int aantal_woorden = 1;

        for (int i = 0; i < zin.length(); i++){
            char letter = zin.charAt(i);
            if(letter == 'a' || letter == 'e' || letter == 'i' || letter == 'o' || letter == 'u'){
                klinkers++;
            }
            else if (letter == ' '){
                aantal_woorden++;
            }
        }
        return new int[]{klinkers, aantal_woorden};
    }

    /* Palindroom checken */
    static boolean palindroom(String zin){
        /* Spaties verwijderen */
        String spatieloos = "";
        for(int i = 0 ; i < zin.length(); i++){
            if(zin.charAt(i) != 32){
                spatieloos = spatieloos + zin.charAt(i);
            }
        }
        /* Controle van spatieloze input op een palindroom */
        int n = spatieloos.length();
        for (int i = 0 ; i < (n / 2) + 1; i++) {
            if (spatieloos.charAt(i) != spatieloos.charAt(n-i-1)) {
            return false;
        }
    }
    return true;
    }

    /* Frequenties */
    static void frequenties(String zin){
        System.out.println("Ter controle de frequenties van alle karakters: ");

        /* 37 karakters (a t/m z + 0 t/m 9 + spatie = 37), op 1e positie het karakter, 2e de frequentie hiervan */
        int alfabet[][] = new int[37][2];

        /* Karakters plaatsen */
        for(int rij = 0; rij < 37; rij++){
            /* a t/m z plaatsen in voorste arrays */
            if (rij >= 0 && rij <= 25) {
                alfabet[rij][0] = rij + 97;
            }
            /* 0 t/m 9 */
            else if (rij >= 25 && rij <= 35) {
                alfabet[rij][0] = rij + 22;
            }
            /* spatie */
            else {
                alfabet[rij][0] = 32;
            }
            /* Frequency plaatsen */
            for(int kolom = 0; kolom < zin.length(); kolom++){
                if ((char)alfabet[rij][0]==zin.charAt(kolom)) {
                    alfabet[rij][1]++;
                }
            }
            System.out.print(alfabet[rij][1] + " ");
        }
        System.out.println("");

        /* Max frequentie waarde */
        double max = 0;
        for(int i = 0; i < 37; i++){
            if(alfabet[i][1] > max){
                max = alfabet[i][1];
            }
        }

        /* Stapgrootte */
        double stapgrootte = 1;
        if(max > 10){
            stapgrootte = max / 10;
        }

        System.out.println("max is " + max + " en stapgrootte " + stapgrootte);
        System.out.println("-------------------------------------------------------------------------");

        /* Staafdiagram */
        for(double rij = max; rij > 0; rij-=stapgrootte){
                for(int kolom = 0; kolom < 37; kolom++){
                    if(alfabet[kolom][1] >= rij){
                        System.out.print("* ");
                    }
                    else {
                        System.out.print("  ");
                    }
                }
                System.out.println("");
        }

        /* Legenda */
        for(int i = 0 ; i < 37; i++){
            System.out.print((char)alfabet[i][0] + " ");
        }
        System.out.println("");
    }
}
