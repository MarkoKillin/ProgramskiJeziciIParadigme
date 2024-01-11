/*
0.
   0.1. Istampati sve brojeve u opsegu [1..10]
   0.2. Istampati sve brojeve u ospegu [1..10)
   0.3 Istampati sve brojeve u opsegu [1..100] da su deljivi sa 3

1. Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
   liste List[List[Any]], a vraca listu brojeva List[Any] tako sto sve elemente
   podlisti spoji u jednu listu. (Koristiti reduce)

   [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9] 

2. Napisati funkciju koja prima listu listi brojeva List[List[Int]], i vraca
   listu suma elemenata listi koje ona sadrzi. Koristiti map i reduce.

   [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5]

3. Napisati funkciju koja prima listu ciji su elementi liste brojeva,
   te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
   neka prazna lista, nju je potrebno izbaciti. Koristiti map i 
   filter

   [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]]

4. Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
   te pomocu funkcija map i reverse "okrene" sve stringove.

   "neki string" -> "gnirts iken"

   ["abc", "dfg", "ert"] -> ["cba", "gfd", "tre"]

5. Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
   iz svake podliste izbacuje elemente deljive sa 3.

6. Napisati funkciju koja prihvata listu listi brojeva, primenjuje na 
   nju prethodnu funkciju i onda izbaci sve podliste koje imaju manje 
   od 5 elemenata.
*/

object Vezbe2 {
    def main(args : Array[String]) : Unit = {
        //zad0()

        //var l = ispeglaj(List(List(1,2,3), List(4,5), List(6), List(7,8), List(), List(9)))
        //for(el <- l) println(el)

        // var l = listaSuma(List(List(1,2,3), List(2,3), List(1,2,4), List(5)))
        // for(el <- l) println(el)

        // var l = izbaciParne(List(List(1,2,3), List(2,4), List(3,4,5), List(7)))
        // for(el <- l){
        //     for(e <- el)
        //         print(e.toString + " ")
        //     println()
        // } 

        // var l = okreni(List("abc", "dfg", "ert"))
        // for(el <- l) println(el)

        // var l = izbaciDeljiveSa3(List(List(1,2,3), List(4,5), List(6), List(7,8), List(6,7), List(9)))
        // for(el <- l){
        //     for(e <- el)
        //         print(e.toString + " ")
        //     println()
        // } 

        var l = izbManjeOd5(List(List(1,2,3,3,4,5,6,7,2), List(4,5,2), List(6), List(7,8,3,2,4,5,5,5), List(6,7), List(9)))
        for(el <- l){
            for(e <- el)
                print(e.toString + " ")
            println()
        } 
    }

    def izbManjeOd5(ll : List[List[Int]]) : List[List[Int]] = {
        izbaciDeljiveSa3(ll).filter(x => x.length >= 5)
    }

    def izbaciDeljiveSa3(ll : List[List[Int]]) : List[List[Int]] = {
        ll.map(x => x.filter(y => y % 3 != 0))
    }

    def okreni(ls : List[String]) : List[String] = {
        ls.map(x => x.reverse)
    }

    def izbaciParne(ll : List[List[Int]]) : List[List[Int]] = {
        var bp = ll.map(x => x.filter(y => y % 2 == 1))
        bp.filter(x => x.length > 0)
    }

    def listaSuma(ll : List[List[Int]]) : List[Int] = {
        ll.map(x => x.reduce((x, y) => x + y))
    }

    def ispeglaj(ll : List[List[Any]]) : List[Any] = {
        ll.reduce((x, y) => x ++ y)
    }

    def zad0() : Unit = {
        for(i <- 1 to 10) print(i)
        println()
        for(i <- 1 until 10) print(i)
        println()
        for(i <- 1 to 100 if (i % 3 == 0)) print(i)
    }
}