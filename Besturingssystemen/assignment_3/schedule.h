/****************************************************************************
   Deze header file bevat de voor de opgave over high-level scheduling en
   geheugen-allocatie benodigde definities

   Auteur:	Dick van Albada
		Vakgroep Computersystemen
		Kruislaan 403
   Datum:	7 septemebr 2003
   Versie:	0.3
****************************************************************************/

/****************************************************************************
   De verschillende soorten events:
   NewProcess_event - er is een nieuw proces in de new_proc rij
	bijgeplaatst.
   Time_event - het lopende proces heeft zijn time-slice opgebruikt.
	Wordt niet gegenereerd, tenzij je zelf "set_slice" aanroept.
	Hoort b.v. in een Round-Robin schedule thuis.
   Ready_event - een proces is klaar met I/O en is weer achteraan de
	ready_proc rij geplaatst. Voor sommige CPU-scheduling algoritmen
	een punt om weer een beslissing te nemen.
   IO_event - het lopende proces gaat I/O doen en is achteraan de
	io_proc rij geplaatst. Als je de volgorde van de processen in
	de io_proc queue of in de ready queue wil aanpassen, kan dat nu.
   Finish_event - het lopende proces is beeindigd en in de defunct_proc rij
	geplaatst. Een goede gelegenheid om nieuwe processen toe te laten.
****************************************************************************/

typedef enum EVENT {NewProcess_event, Time_event, Ready_event, IO_event,
		Finish_event} event_type;

/****************************************************************************
   de structuur "pcb" bevat alle informatie die voor de scheduler
   beschikbaar is.
   SIM_pcb verwijst naar de voor de simulator benodigde informatie en mag
	   niet worden gewijzigd.
   your_admin wordt door de simulator niet gebruikt (aanvankelijk NULL).
	   Deze pointer is beschikbaar om desgewenst voor de scheduler een
	   eigen administratie aan de pcb te kunnen koppelen.
   prev en next worden gebruikt voor het construeren van dubbel verbonden
	   lijsten. Ze kunnen zowel door de simulator als door de scheduler
	   worden gewijzigd.
   MEM_need bevat het aantal voor dit proces benodigde longs geheugen.
	   MEM_need wordt door de simulator ingevuld en mag niet worden
	   gewijzigd.
   MEM_base staat aanvankelijk op -1 en moet door de (hoog-niveau) scheduler
	   een maal worden gevuld met de begin-locatie in het te gebruiken
	   geheugen-array. Door jouw scheduler een maal te veranderen.
   proc_num is het volgnummer van het proces en wordt door de simulator
	   gevuld. Niet veranderen.
****************************************************************************/

typedef struct PCB
{
    void   	*SIM_pcb;
    void   	*your_admin;
    struct PCB 	*prev,
		*next;
    long 	MEM_need,
		MEM_base,
		proc_num;
} pcb;

/****************************************************************************
   De wachtrijen.
   Nieuwe processen worden achteraan in de rij new_proc bijgeplaatst.
	(programma practicumleiding)
   Na toewijzing van geheugen dienen ze in de ready_proc rij te worden
   geplaatst.
	(procedure schedule)
   Een proces wordt voor de CPU gescheduled door het voorin de ready_proc
   rij te plaatsen. Zonder enige verdere voorzieningen werken de io_proc
   en ready_proc rijen op FCFS basis hun klanten af.
	(procedure schedule - een andere CPU scheduling dan FCFS vereist
	dus aanpassingen in de volgorde van processen in deze queues.
   Als een proces I/O wil doen (gesimuleerd), komt het in de io_proc rij.
   Laat deze rij met rust.
   Een beeindigd proces komt in de defunct_proc rij. Ruim deze op.
 *****************************************************************************/

pcb  *new_proc,
     *ready_proc,
     *io_proc,
     *defunct_proc;

/****************************************************************************
   De door de practicum-leiding aangeleverde fucties
*****************************************************************************/

double sim_time();

/****************************************************************************
   sim_time geeft de gesimuleerde "wall-clock time" terug. Gebruik
   naar het je goeddunkt
*****************************************************************************/

extern void set_slice(double slice);

/****************************************************************************
   set_slice zorgt dat over slice tijdseenheden een Time_event optreedt.
   Er kan maar een Time_event tegelijk in de pijp zitten, dus iedere
   set_slice aanroep "overschrijft" de vorige.
   set_slice zorgt er intern voor dat slice steeds minstens 1.0 is, om
   voortgang te garanderen.
   Bij een Time_event wordt voordat de scheduler wordt aangeroepen steeds
   een set_slice(9.9e12) gedaan om te voorkomen dat de simulator daarop kan 
   blijven hangen.
   Gebruik voor deze opgave is niet nodig.
*****************************************************************************/

long    rm_process(pcb **proces);

/****************************************************************************
   rm_process heeft twee taken:
   1. het verzamelt de nodige statistische gegevens over de executie
      van het proces voor het "eindrapport"
   2. het ruimt de pcb en de "SIM_pcb" op en werkt de defunct_proc rij bij.
   rm_process ruimt de eventueel door "your_admin" gebruikte ruimte
   niet op en geeft ook het gereserveerde geheugen niet vrij. Dat
   moet je bij een "Finish_event" zelf doen.
****************************************************************************/

/***************************************************************************
   De functie variabele finale wordt door het hoofdprogramma geinitialiseerd
   met een vrijwel lege functie die alleen de tekst "Einde van het programma"
   afdrukt.
   Hij wordt aangeroepen vlak voor het eind van het programma, als de 
   door het hoofdprogramma verzamelde statistische gegevens zijn afgedrukt.
   Door finale naar een eigen functie van het juiste type te laten verwijzen,
   kun je ervoor zorgen dat je eigen afsluitende routine wordt aangeroepen
   om zo eventuele eigen statistieken af te drukken.
   Een interessante mogelijkheid is b.v. de samenhang tussen de wachttijd op
   geheugen en de grootte van het aangevraagde geheugen te onderzoeken.
   Treedt er starvation op, en zo ja, voor welke processen?
****************************************************************************/

typedef void function();

function *finale;

/****************************************************************************
   Voor de eigenlijke meting worden 100 processen opgestart om de
   queues te vullen. Daarna worden de statistieken weer op nul gezet.
   Definieer zelf een functie waar je reset_stats naar laat wijzen
   om dat ook eventueel voor je eigen statistieken te doen.
****************************************************************************e */

function *reset_stats;

/****************************************************************************
   De door de practicanten te schrijven routine
****************************************************************************e */

void   schedule(event_type event);
