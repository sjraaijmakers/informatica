/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Voorbeeld uitvoer van Datum klasse
 */

import java.util.*;
import java.io.*;

public class Opgave7 {

    private static Scanner scan = new Scanner(System.in);

    public static void main(String [] args) {
        Datum datum1, datum2;

        /* Stap 1: datum 1 */
        System.out.print("Geef een datum (dd-mm-jjjj): ");
        String datumString1 = scan.next();
        while(!correcteDatum(datumString1)){
            System.out.println("Foute invoer, probeer het opnieuw.");
            System.out.println("");
            System.out.print("Geef een datum (dd-mm-jjjj): ");
            datumString1 = scan.next();
        }
        datum1 = new Datum(opdelen(datumString1));

        /* Stap 2: interval */
        System.out.print("Geef een interval (jjjj-jjjj): ");
        String interval = scan.next();
        while(!correctInterval(interval)){
            System.out.println("Onjuist interval, probeer het opnieuw.");
            System.out.println("");
            System.out.print("Geef een interval (jjjj-jjjj): ");
            interval = scan.next();
        }
        String[] jaar = interval.split("-");
        Jaar jaarA = new Jaar(Integer.parseInt(jaar[0]));
        Jaar jaarB = new Jaar(Integer.parseInt(jaar[1]));

        System.out.println("");
        System.out.println(datum1.dag.nummer + " " + datum1.maand.naam + " is een zondag in de volgende jaren: ");
        System.out.println(zondag(jaarA, jaarB, datum1));
        System.out.println("");
        System.out.println("De volledige datum is: " + datum1);
        System.out.println("");
        System.out.println("Het weeknummer is: " + datum1.weekNummer() + "\n");

        /* Stap 3: datum 2 */
        System.out.print("Geef een tweede datum (dd-mm-jjjj ): ");
        String datumString2 = scan.next();
        while(!correcteDatum(datumString2)){
            System.out.println("Foute invoer, probeer het opnieuw.");
            System.out.println("");
            System.out.print("Geef een datum (dd-mm-jjjj): ");
            datumString2 = scan.next();
        }
        datum2 = new Datum(opdelen(datumString2));

        aantallen(datum1, datum2);
    }

    /* Datum controleren; symantisch en juistheid. */
    public static boolean correcteDatum(String datumString) {
        /* Datum controleren op juiste invoer */
        String patroon = "\\d{2}-\\d{2}-\\d{4}";
        if (datumString.matches(patroon)){
            /* Goedgekeurde datum opdelen */
            String [] datumSplit = datumString.split("-");
            Jaar jaar = new Jaar(Integer.parseInt(datumSplit[2]));
            Maand maand = new Maand(Integer.parseInt(datumSplit[1]));
            Dag dag = new Dag(Integer.parseInt(datumSplit[0]));
            Datum tmp = new Datum(dag, maand, jaar);
            /* Goedgekeurde datum controleren op juistheid */
            if(tmp.dag.nummer <= tmp.maand.aantalDagen(tmp.jaar.schrikkelJaar()) && (tmp.maand.nummer >= 1 && tmp.maand.nummer <= 12)){
                return true;
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    }

    /* Datumstring opdelen */
    public static Datum opdelen(String datumString){
        String [] datumSplit = datumString.split("-");

        Jaar jaar = new Jaar(Integer.parseInt(datumSplit[2]));
        Maand maand = new Maand(Integer.parseInt(datumSplit[1]));
        Dag dag = new Dag(Integer.parseInt(datumSplit[0]));

        Datum tmp = new Datum(dag, maand, jaar);
        return tmp;
    }

    /* Jaren interval controleren */
    public static boolean correctInterval(String interval){
        String patroon = "\\d{4}-\\d{4}";
        if (interval.matches(patroon)){
            String[] jaar = interval.split("-");
            Jaar jaarA = new Jaar(Integer.parseInt(jaar[0]));
            Jaar jaarB = new Jaar(Integer.parseInt(jaar[1]));
            if(jaarA.nummer >= 1700 && jaarA.nummer < jaarB.nummer){
                return true;
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    }

    /* Opgegeven datum valt op zondag binnen interval: */
    public static String zondag(Jaar jaarA, Jaar jaarB, Datum datumB){
        String zondagJaar = "";
        for(int i = jaarA.nummer; i <= jaarB.nummer; i++){
            Jaar jaar = new Jaar(i);
            for(int j = 1; j <= 12; j++){
                Maand maand = new Maand(j);
                for(int k = 1; k <= maand.aantalDagen(jaar.schrikkelJaar()); k++){
                    Dag dag = new Dag(k);
                    Datum datumA = new Datum(dag, maand, jaar);
                    if( datumA.dagInWeek()== 6 &&
                        datumA.dag.nummer == datumB.dag.nummer &&
                        datumA.maand.nummer == datumB.maand.nummer){
                        zondagJaar = zondagJaar + datumA.jaar.nummer + " ";
                    }
                }
            }
        }
        return zondagJaar;
    }

    /* Aantallen van waardes tussen twee datums */
    public static void aantallen(Datum datumA, Datum datumB){
        /* Aantal jaren */
        System.out.println("");
        int totaalAantalJaren = datumB.jaar.nummer - datumA.jaar.nummer;

        /* Aantal maanden */
        int totaalAantalMaanden = (datumB.jaar.nummer - datumA.jaar.nummer) * 12;

        /* Aantal dagen (interval van jaren * aantaldagen - datumA (over) - datumB) */
        int totaalAantalDagen = 0;
        for(int i = datumA.jaar.nummer; i <= datumB.jaar.nummer; i++){
            Jaar jaar = new Jaar(i);
            totaalAantalDagen = totaalAantalDagen + jaar.aantalDagen;
        }
        totaalAantalDagen = totaalAantalDagen - datumA.dagInJaar() - (datumB.jaar.aantalDagen - datumB.dagInJaar());

        /* Aantal weken */
        int totaalAantalWeken = totaalAantalDagen / 7;

        /* Printen */
        System.out.println("Totaal aantal jaren:      " + totaalAantalJaren + " jaren");
        System.out.println("Totaal aantal maanden:    " + totaalAantalMaanden + " maanden");
        System.out.println("Totaal aantal weken:      " + totaalAantalWeken + " weken");
        System.out.println("Totaal aantal dagen:      " + totaalAantalDagen + " dagen");
        System.out.println("");
        System.out.println("(deze aantallen zijn naar beneden afgerond)");
    }
}
