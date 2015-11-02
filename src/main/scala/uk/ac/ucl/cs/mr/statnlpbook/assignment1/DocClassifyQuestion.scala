package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File
import uk.ac.ucl.cs.mr.statnlp2015.assignment1.Assignment1Util.Instance
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{Util, UniformLM, LanguageModel}

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
 * @author mbosnjak
 */
object DocClassifyQuestion {

  val word_counts = new HashMap[String,Double] withDefaultValue 0.0

  //An interesting idea:
  //To prevent confusing the ordering of string params..
  val author_word_counts = new HashMap[(String,String),Double] withDefaultValue 0.0
  var authors_list:List[String] = Nil
  var hashmap_count_words = new HashMap[String,Double] withDefaultValue 0.0

  var vocab:Set[String] = Set.empty
  var vocab_list:List[String] = Nil
  def learn(author:String, lyrics:String): Unit = {
    val words = lyrics.split(" ")

    vocab_list ++= words

    for(word <- words) {
      author_word_counts((author, word)) += 1.0
      word_counts(word) += 1.0

    }
  }

  def unigram_probability(word: String, history: Seq[String]): Double = {
    if (vocab(word)) {
      return (word_counts.getOrElse(word,0.0).toDouble / word_counts.foldLeft(0.0)((sum, t) => sum + t._2))
    } else{
      return 0.0
    }
  }

  def classify(instance: Instance): Option[String] = {
    val words = instance.lyrics.split(" ")

    val author_score = new HashMap[String,Double] withDefaultValue 0.0
    for(author <- authors_list) {
      for (word <- words) {
        //Here calculate the prob for each
        //Artist.
        //Append to List
        var denum = if( word_counts(word) > 0) word_counts(word) else 1.0
        author_score(author) += (author_word_counts((author, word)).toDouble) / denum

      }
    }

    val predicted_author = author_score.maxBy(_._2)._1
    return Option(predicted_author)
  }

  def main(args: Array[String]): Unit = {
    // load the datasets

    val train = Assignment1Util.loadDataset(new File("data/p3/p3_train.txt"))
    val dev = Assignment1Util.loadDataset(new File("data/p3/p3_dev.txt"))
    //val test = Assignment1Util.loadDataset(new File(args(2)))


    for(instance <- train) {
      learn(instance.author.get, instance.lyrics)
    }

    this.authors_list = author_word_counts.map(_._1._1).toList.distinct


    // TODO given an instance, how would you classify it


    //train data to a model
      //? How to train?
      // train
    //predict model with dev data,
    authors_list = author_word_counts.map(_._1._1).toList.distinct

    // execute your classifier
    val predictions = dev.map(i => Instance(i.lyrics, i.author, classify(i)))

    // accurately predicted instances
    val accuratelyPredicted = predictions.map(i => i.author.get == i.prediction.get).count(_ == true)

    // total number of instances
    val totalInstances = predictions.length

    // evaluate accuracy
    val accuracy = 1.0 * accuratelyPredicted / totalInstances

    println("classification accuracy:" + accuracy)

  }



}
