/* 
 * Naam:		Steven Raaijmakers
 * Studentnummer: 	10804242
 * Studie: 		Bachelor Informatica
 * Omschrijving: 	Raadspelletje waarbij de gebruiker het door de programma gekozen getal moet raden in drie beurten.
 * Bronnen:		https://github.com/Nisjaat/Javacrap/blob/master/assignment3/Opgave3.java (gebruikt voor lucas getallen met toestemming van assistent)
*/

import java.util.*;

public class Deel4{
	public static void main(String[] args){
		System.out.print("Geef een natuurlijk getal: ");

		/* Gebruikers input in int opslaan */
		Scanner scanner = new Scanner(System.in);
		int natuurlijk_getal = scanner.nextInt();
		
		if(natuurlijk_getal<0){
			System.out.println("Getal negatief, fout");
		}

		else if(natuurlijk_getal>44){
			System.out.println("Getal te groot, past niet");
		}

		/* Als getal positief en niet te groot wordt bevonden */
		else {
			System.out.println("De eerste " + natuurlijk_getal + " Lucas-getallen:");
			
			/* Lucas reeks aanmaken */
			int a = 2;
			int b = 1;
			int c;

			for(int i=0; i<natuurlijk_getal; i++){
				/* Lucas getallen berekenen */
				c = a;
				b = a + b;
				a = b - c;

				System.out.print(a + " ");
			}
			System.out.println("");			
		}


	}
}

