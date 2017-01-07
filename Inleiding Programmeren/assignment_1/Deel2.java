/* 
 * Naam:		Steven Raaijmakers
 * Studentnummer: 	10804242
 * Studie: 		Bachelor Informatica
 * Omschrijving: 	Dit programma toont de som van de even en oneven getallen van 0 t/m een opgegeven getal en tevens het verschil tussen deze twee sommen.
*/

public class Deel2{

	public static void main(String[] args){

		/* Ingevoerd getal naar int converteren */
		int opgegeven = Integer.parseInt(args[0]);

		/* Som van alle oneven getallen vanaf 0 t/m opgegeven getal berekenen */
		int som_oneven = 0;
		for (int i = 1; i <= opgegeven; i+=2){
			som_oneven = som_oneven + i;
		}
		System.out.println("som van oneven getallen tot en met " + opgegeven + " is " + som_oneven);

		/* Som van alle even getallen vanaf 0 t/m opgegeven getal berekenen */
		int som_even = 0;
		for (int i = 0; i <= opgegeven; i+=2){
			som_even = som_even + i;
		}
		System.out.println("som van even getallen tot en met " + opgegeven + " is " + som_even);

		/* Verschil tussen oneven en even */
		int verschil = (som_oneven)-(som_even);		
		System.out.println("verschil tussen twee sommen is " + verschil);
	}
}
