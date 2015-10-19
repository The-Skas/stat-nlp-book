package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File
import uk.ac.ucl.cs.mr.statnlp2015.assignment1.Assignment1Util.Instance
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{UniformLM, LanguageModel}

import scala.collection.mutable.ArrayBuffer

/**
 * @author mbosnjak
 */
object DocClassifyQuestion {

  def main(args: Array[String]): Unit = {
    // load the datasets

    val train = Assignment1Util.loadDataset(new File(args(0)))
    val dev = Assignment1Util.loadDataset(new File(args(1)))
    //val test = Assignment1Util.loadDataset(new File(args(2)))



    // TODO given an instance, how would you classify it
    def classify(instance: Instance) = ???



    // execute your classifier
    val predictions = train.map(i => Instance(i.lyrics, i.author, classify(i)))

    // accurately predicted instances
    val accuratelyPredicted = predictions.map(i => i.author.get == i.prediction.get).count(_ == true)

    // total number of instances
    val totalInstances = predictions.length

    // evaluate accuracy
    val accuracy = 1.0 * accuratelyPredicted / totalInstances

    println("classification accuracy:" + accuracy)

  }



}
