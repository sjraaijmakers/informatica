/* 
 * Naam:		Steven Raaijmakers
 * Studentnummer: 	10804242
 * Studie: 		Bachelor Informatica
 * Omschrijving: 	Raadspelletje waarbij de gebruiker het door de programma gekozen getal moet raden in drie beurten.
*/

import java.util.*;

public class Deel3{

	public static void main(String[] args){

		/* Random getal tussen 0 en 10 nemen */
		Random rand = new Random();
		int nummer = rand.nextInt(10)+1;

		System.out.println("Geef een getal tussen 1 en 10, je mag drie keer raden...");

		Scanner scanner = new Scanner(System.in);
		int input;

		/* Boolean winst is standaard false */
		boolean winst = false;
		
		/* Zolang er nog geen drie beurten zijn */
		for(int i=0; i<3; i++){
			input = scanner.nextInt();
			/* Bij winst verandert boolean winst in true (en stopt loop) */
			if(input==nummer){
				winst=true;
				break;
			}
			else if (input<nummer){
				System.out.println("te klein");
			}
			else if (input>nummer){
				System.out.println("te groot");
			}
		}
		/* Deze code voert pas uit als de for loop is afgelopen */
		if(winst==true){
			System.out.println("Gewonnen!");
		}
		else if(winst==false){
			System.out.println("Verloren, het getal was " + nummer);
		}
	}
}
