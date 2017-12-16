package util

import java.util.concurrent.atomic.LongAdder

class Stats(private val skipFirstDurations: Int) {

  private val countAdder = new LongAdder
  private val totalTimeNsAdder = new LongAdder

  def addDuration(startNanoTime: Long): Unit = {
    val durationNs = System.nanoTime - startNanoTime
    countAdder.increment()
    if (countAdder.intValue() > skipFirstDurations) {
      totalTimeNsAdder.add(durationNs)
    }
  }

  def avgMs: Long = {
    val nonSkippedDurations = count.intValue() - skipFirstDurations
    if (nonSkippedDurations == 0) {
      0
    } else {
      val avgNs = totalTimeNsAdder.sum() / nonSkippedDurations
      avgNs / (1000 * 1000)
    }
  }

  def count: Int = {
    countAdder.intValue()
  }

}
