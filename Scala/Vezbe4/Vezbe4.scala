/*
1. Napisati funkciju koja prima listu tacaka (tacka je par Double-ova)
   i jos jednu "centralnu" tacku (par Double-ova). Iz liste vraca sve tacke
   koja su od centralne tacke udaljene manje od neke prosledjene distance.

2. Napisati funkciju koja privhata listu Stringova, 
   razdvoji svaki po razmaku, te ih sve
   spoji zarezima. Koristiti map i reduce.

3. Napisati funkciju koja prima niz brojeva, one koji su parni uveca za 1,
   a neparne kvadrida. Zatim, svaki od brojeva "okrece" (1234 -> 4321) i 
   na kraju izbacuje sve neparne.

4. Napisati funkciju koja ocekuje listu Int-ova i jos Int n. Za svaki od ostataka
   pri deljenju brojem n, funkcija treba da vrati koliko brojeva iz liste daje taj 
   ostatak. Potrebno je vratiti listu parova oblika:
   
     (ostatak, koliko brojeva iz liste daje ovaj ostatak)

   Primer: List(1, 3, 5, 6, 9), n = 3

   (0, 3),
   (1, 1),
   (2, 1)

5. Definisati apstraktnu klasu Naselje. Naselje moze da bude Selo, Varosica ili Grad (case klase).
   Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double). 
   Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string). Grad 
   ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean).

6. Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
   gradove sa bazenima koji imaju vise od 150 000 stanovnika.
   
*/

object Vezbe4{

   abstract class Naselje(val brStanovnika : Int, val povrsina : Double)
   case class Selo(val a : Int, val b : Double, val zbijeno : String) extends Naselje(a, b)
   case class Varosica(val a : Int, val b : Double) extends Naselje(a, b)
   case class Grad(val a : Int, val b : Double, val bazen : Boolean) extends Naselje(a, b)


   def main(args : Array[String]) = {
      var tacke = List((3.0, 4.0),(2.0, 2.0),(9.0, 4.0),(1.0, 13.0),(3.0, 2.0),(5.0, 5.0));
      // println(prvi(tacke, (2.0, 3.0), 4.0))

      var ls = List("ana voli milovana", "na vrh brda vrba mrda", "riba ribi grize rep")
      // println(drugi(ls))

      // println(treci(Array(15, 13, 6, 18, 22, 25)).toList)

      // println(cetvrti(List(1,3,5,6,9), 3).toList)

      var l: List[Naselje] = List(Selo(150, 25, "zbijeno"),Selo(200, 35, "razbijeno"),Varosica(9000, 50),Grad(200000, 150, false),Grad(200000, 200, true));
      println(sesti(l))
   }

   def sesti(ln : List[Naselje]) : List[Naselje] = {
      ln.filter(filterSesti)
   }

   def filterSesti(n : Naselje) : Boolean = {
      n match {
         case Selo(a,b,c) => c.equals("razbijeno")
         case Grad(a,b,c) => c && a > 150000
         case _ => false
      }
   }

   def cetvrti(li : List[Int], n : Int) : List[Tuple2[Int, Int]] = {
      var ostaci = new Array[Int](n)
      for(br <- li){
         var os = br % n
         ostaci(os) = ostaci(os) + 1
      }
      for(i <- (0 until n).toList) 
         yield (i, ostaci(i))
   }

   def treci(nb : Array[Int]) : Array[Int] = {
      nb.map(treciMap).map(treciOkreni).filter(x => x % 2 == 0)
   }

   def treciOkreni(i : Int) : Int = {
      var kopija = i
      var okrenut = 0
      while(kopija > 10){
         var cifra = kopija % 10
         kopija /= 10
         okrenut = okrenut * 10 + cifra
      }
      okrenut
   }

   def treciMap(i : Int) : Int = {
      if(i % 2 == 0) {
         i + 1
      } else {
         i * i
      }
   }

   def drugi(ls : List[String]) : String = {
      ls.flatMap(x => x.split(" ")).reduce((x, y) => x + ", "  + y)
   }

   def prvi(listaTacaka : List[Tuple2[Double, Double]], tacka : Tuple2[Double, Double], d : Double) : List[Tuple2[Double, Double]] = {
      listaTacaka.filter(prviFilter(tacka, d))
   }

   def prviFilter(t1 : Tuple2[Double, Double], d : Double) (t2 : Tuple2[Double, Double]) : Boolean = {
         var dis = Math.sqrt((t1._1 - t2._1)*(t1._1 - t2._1) + (t1._2 - t2._2)*(t1._2 - t2._2))
         dis < d
   }
}