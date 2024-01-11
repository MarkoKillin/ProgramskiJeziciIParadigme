/*
1. Definisati apstraktnu klasu Stednja koja predstavlja stedni racun u banci. 
   Stednja moze da bude DinarskaStednja ili DeviznaStednja (case klase). Obe 
   stednje kao osobine imaju kamatnu stopu (double) i informaciju da li su 
   orocene ili neorocene (boolean). DeviznaStednja, za razliku od Dinarske, 
   takodje ima boolean koji predstavlja da li je u evrima ili nije. 

2. Napisati funkciju koja iz liste Stednji vraca dinarske stednje koje
   koje imaju kamatnu stopu vecu od 2.

3. Napisati funkciju koja iz liste stednji vraca dinarske stednje koje su
   orocene i devizne stednje koje su u evrima.

4. Napisati funkciju koja ocekuje listu stednji i racuna prosek kamatnih stopa
   stednji koje su orocene.

*/

object Domaci3 {
   abstract class Stednja(var kamatnaStopa : Double, var orocena : Boolean)
   case class DinarskaStednja(var ks : Double, var o : Boolean) extends Stednja(ks, o)
   case class DeviznaStednja(var ks : Double, var o : Boolean, var uEvrima : Boolean) extends Stednja(ks, o)

   def main(args : Array[String]) = {
      var l = List(DinarskaStednja(3.0, true), DinarskaStednja(1.0, false), DeviznaStednja(1.2, false, true), DeviznaStednja(2.0, true, false));
      //println(vratiDinarske(l))

      // println(dinarskeOrocene(l))

      println(cetvrti(l))
   }

   def cetvrti(ls : List[Stednja]) : Double = {
      var sum = 0.0
      var cnt = 0
      for(s <- ls){
         if(s.orocena){
            sum += s.kamatnaStopa
            cnt += 1
         }
      }
      if(cnt == 0)
         0
      else
         sum/cnt
   }

   def dinarskeOrocene(ls : List[Stednja]) : List[Stednja] = {
      ls.filter(filtrirajDinarskeOrocene)
   }

   def filtrirajDinarskeOrocene(s : Stednja) : Boolean = {
      s match {
         case DinarskaStednja(ks, o) => o
         case DeviznaStednja(ks, o, e) => e
      }
   }

   def vratiDinarske(ls : List[Stednja]) : List[Stednja] = {
      ls.filter(filtrirajDinarske)
   }

   def filtrirajDinarske(s : Stednja) : Boolean = {
      s match {
         case DinarskaStednja(ks, o) => if (ks > 2) true else false
         case _ => false
      }
   }
}