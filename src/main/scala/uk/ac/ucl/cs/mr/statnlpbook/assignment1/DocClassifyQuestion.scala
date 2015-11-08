package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File
import uk.ac.ucl.cs.mr.statnlp2015.assignment1.Assignment1Util.Instance
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{Util, UniformLM, LanguageModel}

import scala.collection.mutable.{HashMap, ArrayBuffer}
import collection.mutable.{Map => MMap}
/**
 * @author mbosnjak
 */
object DocClassifyQuestion {


  //An interesting idea:
  //To prevent confusing the ordering of string params..
  val author_word_counts = HashMap.empty[String,MMap[String,Double]]
  val author_total_word_counts = new HashMap[String, Double] withDefaultValue 0.0
  //name -> song_counts
  val author_song_counts = new HashMap[String, Double] withDefaultValue 0.0

  var authors_list:List[String] = Nil
  var hashmap_count_words = new HashMap[String,Double] withDefaultValue 0.0

  var vocab:Set[String] = Set.empty
  def learn(author:String, lyrics:String): Unit = {
    val words = lyrics.split(" ")

    //Increment the number of songs the author has.
    author_song_counts(author) += 1.0

    for(word <- words) {
      author_word_counts.getOrElseUpdate(author, HashMap.empty[String,Double].withDefaultValue(0.0))(word) += 1.0

      //The total count of all words for an author.
      author_total_word_counts(author) += 1.0

      //To gain a list of all unique words
      vocab += word

    }
  }


  def classify(instance: Instance): Option[String] = {
    val words = instance.lyrics.split(" ")

    val author_score = new HashMap[String,Double] withDefaultValue 0.0

    val vocab_size:Double = vocab.size.toDouble

    for(author <- authors_list) {
      for (word <- words) {
        //Here calculate the prob for each
        //Artist.
        //Append to List
        if(vocab(word)) {
          var denum = author_total_word_counts(author)

          //Apply laplace smoothing
          author_score(author) += Math.log((author_word_counts(author)(word).toDouble + 1.0)
                                                            /
                                                    (denum + vocab_size))
        }

      }
        //Here add author probability
      val total_songs= author_song_counts.map(_._2).sum
      val author_probability = author_song_counts(author).toDouble / total_songs

      author_score(author) += Math.log(author_probability)


        //Some log author probability
        //Add Smoothing.
        //Equation:
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

    this.authors_list = author_word_counts.map(_._1).toList.distinct


    // TODO given an instance, how would you classify it


    //train data to a model
      //? How to train?
      // train
    //predict model with dev data,
    authors_list = author_word_counts.map(_._1).toList.distinct

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
