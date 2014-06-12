    def compareInts(first: Int, second: Int): Int =
        if (first > second) 1
        else if (first == second) 0
        else -1

    val heap = new Heap[Int](compareInts)

    for (i <- 0 to 3) {
        heap.add((Math.random() * 100).toInt)
    }
    heap
    heap.pop
    heap.pop
    heap.pop
    heap.pop


    def compareStrings(first: String, second: String): Int =
        if (first > second) 1
        else if (first == second) 0
        else -1
    var heap2 = new Heap[String](compareStrings)
    heap2.add("a")
    heap2.add("cc")
    heap2.add("")

    heap2.pop
    heap2.pop
    heap2.pop
    heap2.pop