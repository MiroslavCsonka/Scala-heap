
package heap

import scala.collection.mutable.ListBuffer

class Heap[T](compareFunction: (T, T) => Int) {

    type node = (Int, T)

    private val HIGHER = 1
    private val LOWER = -1
    private val heapRepresentation = new ListBuffer[T]()

    def add(item: T): Heap[T] = {
        heapRepresentation.append(item)
        bubbleUp(lastIndex)
        this
    }

    def pop: Option[T] = heapRepresentation.size match {
        case 0 => None
        case _ =>

            val firstValue = heapRepresentation(0)
            if (heapRepresentation.size == 1) {
                heapRepresentation.remove(0)
                return Some(firstValue)
            }
            swap(0, lastIndex)
            heapRepresentation.remove(lastIndex)
            bubbleDown(0)
            Some(firstValue)
    }

    private def lastIndex = heapRepresentation.size - 1

    private def valueOf(index: Int): T = heapRepresentation(index)

    private def swap(index1: Int, index2: Int) {
        val temp = valueOf(index1)
        heapRepresentation.update(index1, valueOf(index2))
        heapRepresentation.update(index2, temp)
    }

    private def bubbleUp(currentIndex: Int) {
        def getParent(i: Int) = (i - 1) / 2

        if (currentIndex > 0) {
            val parentIndex = getParent(currentIndex)

            compareFunction(valueOf(currentIndex), valueOf(parentIndex)) match {
                case LOWER =>
                    swap(currentIndex, parentIndex)
                    bubbleUp(parentIndex)
                case _ =>
            }
        }
    }

    private def bubbleDown(currentIndex: Int) {
        getLowerChild(currentIndex) match {
            case Some((lowerChildIndex, lowerChildValue)) =>
                if (compareFunction(valueOf(currentIndex), lowerChildValue) == HIGHER) {
                    swap(currentIndex, lowerChildIndex)
                    bubbleDown(lowerChildIndex)
                }
            case None =>
        }
    }

    private def getLowerChild(index: Int): Option[node] = {
        def getChildrenIndices(parentIndex: Int): (Int, Int) = (2 * parentIndex + 1, 2 * parentIndex + 2)

        val (leftChildIndex, rightChildIndex) = getChildrenIndices(index)

        val areChildrenInBoundsOfHeap = (rightChildIndex <= lastIndex) && (leftChildIndex <= lastIndex)
        if (!areChildrenInBoundsOfHeap) return None

        val (leftChildValue, rightChildValue) = (heapRepresentation(leftChildIndex), heapRepresentation(rightChildIndex))

        compareFunction(leftChildValue, rightChildValue) match {
            case LOWER => Some((leftChildIndex, leftChildValue))
            case _ => Some((rightChildIndex, rightChildValue))
        }
    }

    override def toString = {
        s"[$heapRepresentation](${heapRepresentation.size})"
    }
}