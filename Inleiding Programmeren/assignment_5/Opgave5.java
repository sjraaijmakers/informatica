/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Programma simuleert een Hotel administratie systeem waarbij gasten kunnen in- en uitchecken.
*/

import java.util.*;

public class Opgave5{

    private static Scanner scan = new Scanner(System.in);

    /* Main */
    public static void main(String[] args) {
        try {
            Hotel h = new Hotel(Integer.parseInt(args[0]));
            gebruikersInvoer(h);
        }
        catch (IndexOutOfBoundsException e) {
            System.err.println("Er is iets misgegaan.");
        }
    }

    /* Gebruikers menu */
    public static void gebruikersInvoer(Hotel hotel) {
        boolean stop = false;
        do {
            System.out.println("");
            System.out.println("Kies uit [1] Statusoverzicht, [2] Gasten inchecken, [3] Gasten uitchecken, [4] Einde");
            System.out.print("Uw keuze: ");
            int choice = controleNummer();

            /* Status overzicht van hotel */
            if(choice == 1){
                hotel.overzicht();
            }

            /* Inchecken in hotel */
            else if(choice == 2){
                /* Lege kamer zoeken */
                if(hotel.vrijeKamer() > 0){
                    System.out.print("Geef achternaam gast: ");
                    String achternaam = scan.next();
                    System.out.print("Geef voornaam gast: ");
                    String voornaam = scan.next();
                    System.out.print("Geef geboortedatum gast [dd-mm-jjjj]: ");
                    Datum geboorteDatum = new Datum(scan.next());
                    if(geboorteDatum.toString()!=""){
                        Gast gast = new Gast(achternaam, voornaam, geboorteDatum);
                        hotel.inchecken(gast, (hotel.vrijeKamer() - 1));
                    }
                }
                else {
                    System.out.println("Helaas is er geen kamer vrij.");
                }
            }

            /* Uitchecken bij hotel */
            else if(choice == 3){
                Scanner scan = new Scanner(System.in);
                System.out.print("In welke kamer heeft u geslapen? ");
                int uitcheckKamer = controleNummer()-1;
                hotel.uitchecken(uitcheckKamer);
            }

            /* Einde hotel */
            else if(choice == 4){
                stop = true;
            }
        }
        while(stop != true);
    }

    /* Gebruikersinput controleren */
    static int controleNummer () {
        int choice = 0;
        if (scan.hasNextInt()){
            choice = scan.nextInt();
        }
        else {
            System.out.println("Geen geldige gebruikers invoer!");
            choice = 4;
        }
        return choice;
    }

}
