/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Interface beschrijft vereisten van klasse Breuk.
 */

interface BreukInterface {
    public Breuk telop(Breuk cg);
    public Breuk trekaf(Breuk cg);
    public Breuk vermenigvuldig(Breuk cg);
    public Breuk deel(Breuk cg);
    public Breuk omgekeerde();
}
