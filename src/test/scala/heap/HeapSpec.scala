package heap

import org.scalatest._


class HeapSpec extends FlatSpec with Matchers {

    val emptyHeap = new Heap[Int](compareInts)

    def compareInts(first: Int, second: Int): Int =
        if (first > second) 1
        else if (first == second) 0
        else -1


    "Empty heap" should "pop None" in {
        emptyHeap.pop shouldBe None
    }

    "Heap with 1 element" should "pop that element" in {
        emptyHeap.add(5).pop shouldBe Some(5)
    }

    "Heap with 2 elements" should "pop smaller element" in {
        val filledHeap = emptyHeap.add(5).add(1)

        filledHeap.pop shouldBe Some(1)
        filledHeap.pop shouldBe Some(5)
    }

    "Heap with many elements" should "pop the smallest " in {
        val filledHeap = emptyHeap.add(5).add(1).add(7).add(0).add(-1).add(30)

        filledHeap.pop shouldBe Some(-1)
        filledHeap.pop shouldBe Some(0)
        filledHeap.pop shouldBe Some(1)
        filledHeap.pop shouldBe Some(5)
        filledHeap.pop shouldBe Some(7)
        filledHeap.pop shouldBe Some(30)
    }
}
