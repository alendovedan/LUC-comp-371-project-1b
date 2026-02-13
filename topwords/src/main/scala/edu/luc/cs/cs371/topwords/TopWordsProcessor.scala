package edu.luc.cs.cs371.topwords

import scala.collection.mutable

//stdin args + Observer Object 
class TopWordsProcessor(
  cloudSize: Int,
  minLength: Int,
  windowSize: Int,
  observer: WordCloudObserver
  ):
  private val window = mutable.Queue[String]()//FIFO
  private val freqs = mutable.Map[String, Int]().withDefaultValue(0)
  private var wordCount = 0

  def processWord(word: String): Unit =
    //word length not long enough to count 
    if word.length < minLength then
      return
    //add word to end of window and increase counters
    window.enqueue(word)
    freqs(word) += 1
    wordCount+=1 
    if window.size > windowSize then
      val evict = window.dequeue()
      freqs(evict) -= 1
      if (freqs(evict) == 0) then 
        freqs.remove(evict)//if window is full remove oldest word
    if wordCount >= windowSize then
      val wordCloud = generateWordCloud()
      observer.onUpdate(wordCloud)



  private def generateWordCloud(): List[(String, Int)] =
    freqs
      .toList
      .sortBy(-_._2)//sort by frequency descending
      .take(cloudSize)
