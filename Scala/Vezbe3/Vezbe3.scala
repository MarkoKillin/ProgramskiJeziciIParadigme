/*
1. Definisati apstraktnu klasu Skola sa val poljima ime 
   i broj ucenika. Definisati case klase OsnovnaSkola i
   SrednjaSkola koje nasledjuju klasu Skola, primaju potrebne
   parametre i prosledjuju ih konstruktoru nadklase.

2. Napisati funkciju koja prima listu skola, 
   te za svaku skolu, po tipu, ispise poruku
   "Osnovna/Srednja skola (ime) ima (broj) ucenika."

3. Napisati funkciju koja ce iz liste skola izbaciti sve
   skole koje imaju manje od 300 ucenika, koristeci funkciju
   filter.

4. Definisati apstraktnu klasu NebeskoTelo sa val poljima
   ime i precnik (u km). Nakon toga definisati case klase Planeta
   i Satelit koji nasledjuju apstraktnu klasu NebeskoTelo. 
   Planeta takodje ima polje "imaVodu", a Satelit ima polje
   "kruziOko" (String) koje sadrzi ime planete oko koje kruzi.

5. Napisati funkciju koja pomocu filter funkcije vrati samo one
   Satelite koji se nalaze u orbiti oko planete cije ime se prosledi
   kao parametar.

6. Napisati funkciju koja vraca sve Planete koje imaju vodu i imaju 
   precnik veci od 5500 km. Takodje, treba da vrati sve satelite sa
   precnikom manjim od 1000 km.
*/


object Vezbe3 {
   abstract class Skola (val ime : String, val brojUcenika : Int) 
   case class OsnovnaSkola (val a : String, val b : Int) extends Skola(a, b)
   case class SrednjaSkola (val a : String, val b : Int) extends Skola(a, b)

   abstract class NebeskoTelo(val ime : String, val precnik : Double)
   case class Planeta(val i : String, val p : Double, val imaVodu : Boolean) extends NebeskoTelo(i, p)
   case class Satelit(val i : String, val p : Double, val kruziOko : String) extends NebeskoTelo(i, p)

   def main(args : Array[String]) = {
      val listaSkola = List(new OsnovnaSkola("a", 350), new SrednjaSkola("b", 243), new SrednjaSkola("ss", 664))
      // skole(listaSkola)

      // for(s <- izbaciSkole(listaSkola)) {
      //    println(f"Skola ${s.ime} ima ${s.brojUcenika} ucenika.")
      // }

      val lnt = List(new Planeta("Zemlja", 6500, true), new Satelit("Mesec", 1700, "Zemlja"), new Planeta("Jupiter", 70000, false), new Satelit("Evropa", 1600, "Jupiter"), new Satelit("Io", 1800, "Jupiter"))
      // println(satelitiPlanete(lnt, "Zemlja"))

      println(vratiPlanete(lnt))
   }

   def vratiPlanete(lnt : List[NebeskoTelo]) : List[NebeskoTelo] = {
      lnt.filter(filtriraj)
   }

   def filtriraj(nt : NebeskoTelo) : Boolean = {
      nt match {
         case Planeta(ime, precnik, imaVodu) => imaVodu && precnik > 5500
         case Satelit(ime, precnik, kruziOko) => precnik < 1000
      }
   }

   def satelitiPlanete(lnt : List[NebeskoTelo], ip : String) : List[NebeskoTelo] = {
      lnt.filter(proveriSatelit(ip))
   }

   def proveriSatelit(ip : String) (nt : NebeskoTelo) : Boolean = {
      nt match {
         case Satelit(a, b, planeta) => planeta.equals(ip)
         case _ => false 
      }
   }

   def izbaciSkole(ls : List[Skola]) : List[Skola] = {
      ls.filter(x => x.brojUcenika >= 300)
   }

   def skole(ls : List[Skola]) : Unit = {
      for(s <- ls){
         s match {
            case OsnovnaSkola(ime, brojUcenika) => println(f"Osnovna skola ${s.ime} ima ${s.brojUcenika} ucenika.")
            case SrednjaSkola(ime, brojUcenika) => println(f"Srednja skola ${s.ime} ima ${s.brojUcenika} ucenika.")
         }
      }
   }
}