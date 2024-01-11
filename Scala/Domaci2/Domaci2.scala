/*
1. Definisati funkciju koja prima listu brojeva i ukoliko je
   ona parne duzine, svaki element kvadrira, inace svaki element
   mnozi sa 10. Koristiti funkciju map.

2. Napisati funkciju koja prima niz karaktera. Prvo je potrebno
   funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
   map pretvoriti sva preostala slova u velika.

3. Definisati funkciju prosecnaDuzina koja prima niz stringova
   i racuna njihovu prosecnu duzinu pomocu funkcije reduce i map.

4. Definisati funkciju koja prima niz brojeva. Zadrzi one brojeve
   kojima je prva i poslednja cifra ista, te svaki broj "okrene"
   po principu: "12341" -> "14321". Koristiti map i filter.

5. Definisati funkciju koja prima niz stringova. Treba da izbaciti
   sve one koji imaju vise od 4 velika slova. Nakon toga potrebno
   je na svaki od njih nalepiti sebe u "ogledalu" ("abc" -> 
   "abccba") i na kraju ih sve spojiti u jedan veliki string. 
   Koristiti map, filter, reduce.
*/


object Domaci2 {
    def main(args: Array[String]) = {
        // var l = prvi(List(1,2,3,4,5,6))
        // for(i <- l) println(i.toString + " ")

        // var l = drugi(Array('a','B','c'))
        // for(i <- l) println(i.toString + " ")

        //println(prosecnaDuzina(Array("aaaa", "ababababb", "")))

        //print(cetvrti(Array(123456,1234233331,525235,231435252)).toList)

        print(peti(Array("Marko", "DAniLOS", "stevan", "Miodrage")))
    }

    def peti(niz : Array[String]) = {
        def velika4(s : String) : Boolean = {
            var i = 0
            for(c <- s.toCharArray)
                if(c.isUpper)
                    i += 1
            
            if(i > 4)
                false
            else 
                true
        }
        niz.filter(velika4).map(x => x + x.reverse).reduce(_ + _)
    }

    def cetvrti(niz : Array[Int]) : Array[Int] = {
        def ist(x : Int) : Boolean = {
            if(x < 10)
                true
            else {
                var pos = x % 10
                var n = x
                while(n > 9){
                    n /= 10
                }
                n == pos
            }
        }

        def okreni(x : Int) : Int = {
            var ok = 0
            var n = x
            while(n > 0){
                ok = ok * 10 + n % 10
                n /= 10
            }
            ok
        }

        niz.filter(ist).map(okreni)
    }



    def prosecnaDuzina(niz : Array[String]) : Double = {
        var n = niz.length
        var s = niz.map(x => x.length).reduce(_ + _)
        s / (1.0 * n)
    }

    def drugi(nc : Array[Char]) : Array[Char] = {
        nc.filter(x => x.isLower).map(x => x.toUpper)
    }

    def prvi(l : List[Int]) : List[Int] = {
        if(l.length % 2 == 0){
            return l.map(x => x * x)
        } else {
            return l.map(x => x * 10)
        }
    }
}