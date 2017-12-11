

object Day6 extends App {

  val inputraw = "14\t0\t15\t12\t11\t11\t3\t5\t1\t6\t8\t4\t9\t1\t8\t4"
  var banks = inputraw.split("\t").toList.map(_.toInt)
//  var banks = List(0, 2, 7, 0)

  type State = List[Int]

  def getBankWithMostBlocks(banks: List[Int]) = banks.zipWithIndex.maxBy(_._1)

  def reallocate(banks: List[Int], startIndex: Int, blocksToDistribute: Int, blocksPerBank: Int): List[Int] = {
    // Take the blocks out of the bank that is to be redistributed
    val newbanks = banks.updated(startIndex, 0)

    // Distribute the blocks from this bank over all blocks
    reallocateBlocks(newbanks, (startIndex + 1) % banks.size, blocksToDistribute, blocksPerBank)
  }

  def reallocateBlocks(banks: List[Int], currentIndex: Int, blocksToDistribute: Int, blocksPerBank: Int): List[Int] = {
    assert(blocksToDistribute>=0)

    if (blocksToDistribute == 0) banks
    else {
      val blocksToAdd = Math.min(blocksPerBank, blocksToDistribute)
//      println("reallocateBlocks \t\t banks: %s \t currentIndex: %s \t blocksToDistribute: %s \t blocksToAdd: %s \t blocksPerBank: %s".format(banks, currentIndex, blocksToDistribute, blocksToAdd, blocksPerBank))
//      println("statesSeen: " + statesSeen)
      // Update a bank and add some blocks to it
      val newbanks = banks.updated(currentIndex, banks(currentIndex) + blocksToAdd)

      val nextIndex = (currentIndex + 1) % banks.size // be sure to wrap around index
      val blocksLeftToDistribute = Math.max(blocksToDistribute - blocksPerBank, 0)
      reallocateBlocks(newbanks, nextIndex, blocksLeftToDistribute, blocksPerBank)
    }
  }

  var steps = 0
  var statesSeen = scala.collection.mutable.Map[State, Int]()

  while(true) {
    val highest = getBankWithMostBlocks(banks)
    val highestNumBlocks = highest._1
    val highestIndex = highest._2
    val blocksPerBank = Math.ceil(highestNumBlocks.toDouble / banks.size).toInt

    banks = reallocate(banks, highestIndex, highestNumBlocks, blocksPerBank)
    println("Finished one iteration. Totals steps %s".format(steps))
    steps += 1

    val previous = statesSeen.put(banks, steps)
    if(previous.isDefined) {
        // Calculate and print # redistribution cycles in the loop
        println("previous: " + previous.get)
        println("Loop cycles: " + (steps - previous.get))
        System.exit(0)
    }
  }

  println("steps " + steps)


}
