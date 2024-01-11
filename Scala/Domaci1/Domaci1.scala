import scala.io.StdIn._
/*
* Za zadatke [1-4] ucitavati podatke od korisnika.
1. Napisati funkciju koja prebrojava koliko elementa niza 
   je parno.
2. Napisati funkciju koja prihvata niz string-ova i vraca 
   samo one koji su duzi od nekog n.

3. Napisati funkciju koja prima niz brojeva i vraca sve kombinacije
   parova brojeva tog niza (Tuple2[Int, Int]). Nakon toga, definisati
   funkciju koja iz niza parova vraca samo one cija srednja vrednost
   upada u neki interval [a..b].

4. Napisati funkciju koja vraca skalarni proizvod dva niza.
5. Napisati funkciju koja prebrojava koliko elemenata liste 
   l je manje od nekog n.

6. Napisati funkciju koja prihvata listu i vraca listu
   svih prostih brojeva iz prvobitne liste.

7. Napisati quicksort za sortiranje listi.
*/
object Domaci1 {


    def main(args : Array[String]) = {
        // println("Unesi n")
        // var n = readInt
        // var niz = new Array[Int](n)
        // print("Unesi niz")
        // for(i <- 0 until n)
        //     niz(i) = readInt
        // print(prebrojParne(niz))

        // println("Unesi n")
        // var n = readInt
        // println("Unesi g")
        // var g = readInt
        // var niz = new Array[String](n)
        // print("Unesi niz")
        // for(i <- 0 until n)
        //     niz(i) = readLine
        // for(s <- vratiDuze(niz, g))
        //     print(s + " ")

        // var arr = parovi(Array.range(0, 11))
        // for(x <- poveriParove(arr, 3.5, 4.5)) println(x)

        //println(skalarni(Array.range(0, 5), Array.range(10,15)))

        // println(prebroj(List.range(0,50), 23))

        // var l = listaProstih(List.range(1,100))
        // for(e <- l) println(e)

        for(e <- quicksort(List(2,3,5,1,3,5,7,8,3,2))) println(e)
    }

    def quicksort(l : List[Int]) : List[Int] = {
        if(l == Nil || l.tail == Nil) return l

        var manji = ltp(l.head, l.tail)
        var veci = gtp(l.head, l.tail)

        manji = quicksort(manji)
        veci = quicksort(veci)

        return manji ::: List(l.head) ::: veci
    }

    def ltp(pivot : Int, l : List[Int]) : List[Int] = {
        for(el <- l if (el <= pivot)) yield el
    }

    def gtp(pivot : Int, l : List[Int]) : List[Int] = {
        for(el <- l if (el > pivot)) yield el
    }

    def listaProstih(l : List[Int]) : List[Int] = {
        l match {
            case Nil    => Nil
            case h :: t => if(jeProst(h)) h :: listaProstih(t)
                           else listaProstih(t)
        }
    }

    def jeProst(el : Int) : Boolean = {
        for(i <- 2 to (el/2)){
            if(el % i == 0)
                return false
        }
        return true
            
    }

    def prebroj(l : List[Int], n : Int) : Int = {
        l match {
            case Nil   => 0
            case h :: t => if(h < n) 1 + prebroj(t,n)
                          else prebroj(t,n)
        }
    }

    def skalarni(arr1 : Array[Int], arr2 : Array[Int]) : Int = {
        if(arr1.length == arr2.length){
            var sum = 0
            for(i <- 0 until arr1.length){
                sum += (arr1(i) * arr2(i))
            }
            sum
        } else {
            println("GRESKA, NIZOVI NISU ISTE DUZINE")
            -1
        }
    }

    def parovi(arr : Array[Int]) : Array[Tuple2[Int, Int]] = {
        for(x <- arr; y <- arr) yield (x, y)
    }

    def poveriParove(arr : Array[Tuple2[Int, Int]], a : Double, b : Double) : Array[Tuple2[Int, Int]] = {
        for(par <- arr; sr = (par._1 + par._2)/2.0 if (a <= sr && sr <= b)) yield par
    }

    def vratiDuze(arr : Array[String], n : Int) : Array[String] = {
        for(s <- arr if s.length > n)
            yield s
    }

    def prebrojParne(arr : Array[Int]) : Int = {
        var i = 0
        for(x <- arr){
            if(x % 2 == 0)
                i = i + 1
        }
        i
    }
}