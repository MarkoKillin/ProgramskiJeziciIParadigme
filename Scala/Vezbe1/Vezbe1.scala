import scala.io.StdIn._
/*
1. Napisati funkciju tipKaraktera koja prima Char, odredjuje da li je
   on veliko, malo ili nije slovo uopste, i vraca poruku kao String.

2. Napisati funkciju pomnoziSa koja prima Int i niz Int-ova,
   a kao rezultat vraca prvi broj pomnozen sa svim brojevima
   niza.

3. Napisati funkciju koja kvadrira sve brojeve nekog niza. (Koristiti
   yield).

4. Napisati funkciju koja prima Char i Int. U while petlji ispisuje
   karakter c, n puta.

5. Napisati funkciju kalkulator, koja prihvata 2 Int-a i Char
   (+, -, *, /), i pomocu match-case utvrdjuje koju operaciju
   treba da izvrsi. Za nepoznatu operaciju vratiti 0. Ucitati
   podatke od korisnika.

6. Napisati funkciju koja utvrdjuje da li je prosledjeni argument nekog
   brojevnog tipa (Int, Float...) ili nije.

7. Napisati funkciju koja prima par Int-ova i broj n,
   vraca novi par gde je prvi element para pomnozen sa n
   a drugom elementu para je dodat broj n.

8. Napisati funkciju "spljosti" koja radi sa listama.

	[1, 2, 2, 2, 3, 3, 4, 5, 5, 5] -> [1, 2, 3, 4, 5]
*/

object Vezbe1 {

    def spljosti(l : List[Int]) : List[Int] = {
        l match {
            case Nil => Nil
            case x :: Nil => x :: Nil
            case x :: y :: tail =>
                if (x == y)
                    spljosti(y :: tail)
                else
                    x :: spljosti(y :: tail)
        }
    }

    def main(args: Array[String]) : Unit = {
        // print("Unesi slovo: ")
        // println(tipKaraktera(readChar()))

        // println(pomnoziSa(1, Array(1,2,3,4,5)))

        // for(x <- kvadrira(Array(1,2,3,4,5)))
        //     print(x.toString + " ")

        //ispisuje('a',20)

        //print(kalkulator(readInt, readInt, readChar))

        //print(utvrdi(2.5))

        //print(parovi((2,3),5))

        for(x <- spljosti(List(1,2,2,2,3,3,4,5,5,5)))
            print(x.toString + " ")
    }

    def parovi(x : (Int, Int), n : Int) : (Int, Int) = {
        (x._1 * n, x._2 + n)
    }

    def utvrdi(x : Any) : String = {
        x match {
            case x : Int => "Int je"
            case x : Float => "Float je"
            case x : Double => "Double je"
            case _ => "Nije broj"
        }
    }

    def kalkulator(x : Int, y : Int, c : Char) : Double = {
        c match {
            case '+' => x + y
            case '-' => x - y
            case '*' => x * y
            case '/' => x / (1.0 * y)
            case _   => 0
        }
    }

    def ispisuje(c : Char, n : Int) : Unit = {
        var i = 0
        while (i < n) {
            print(c)
            i = i + 1
        }
    }

    def kvadrira(arr : Array[Int]) : Array[Int] = {
        return for(x <- arr) yield x * x
    }

    def pomnoziSa(x : Int, arr : Array[Int]) : Int = {
        var p = x
        for(a <- arr) {
            p = p * a
        }
        return p
    }

    def tipKaraktera(c : Char) : String = {
        if (c >= 'A' && c <= 'Z')     
            return "Veliko slovo"
        else if (c >= 'a' && c <= 'z') 
            return "Malo slovo"
        else                           
            return "Nije slovo"
        
    }
}