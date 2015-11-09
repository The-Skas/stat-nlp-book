package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlp2015.assignment1.BarQuestion.MyBarAwareLM
import uk.ac.ucl.cs.mr.statnlpbook.Segmenter
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{Util, LanguageModel}

/**
 * @author riedel
 */
object OpenEndedLMQuestion {

  /**
   * An LM that performs well on the dev set.
   */


  case class MyReallyGooDLM(train:IndexedSeq[String], _vocab: Set[String]) extends LanguageModel
  {
    //The vocabulary words.
    def vocab = _vocab
    def order = 25

    //Weight that can be modified.
    var alpha:Double = 3.0
    var beta:Double = 0.26

    def some = Segmenter
    val states: Map[String, Int] = Map("UNIFORM" -> 0
      , "LINEAR" -> 1, "END-LINEAR" -> 2);

    var curr_state:Int = states("END-LINEAR")

    def uniform_probability(word: String, history: Seq[String]): Double =  {
      if (vocab(word)) {
        if(word.contentEquals("[BAR]")) {
          count_distance = 0
          curr_state = states("LINEAR")
          //Double Check
        }else if(history.contains("[BAR]") && !history.contains("[/BAR]")){
          //Doent work in some cases where bar and not bar are in the history eeep, so wrong atm BUT
          // this block only should occur when initialized so its fine.
          count_distance = 0
          curr_state = states("LINEAR")
        }

        else if(word.contentEquals("[/BAR]")){
          count_distance = 0
          curr_state = states("END-LINEAR")
        }else if(history.contains("[/BAR]") && !history.contains("[BAR]")){
          count_distance = 0
          curr_state = states("END-LINEAR")
        }


        return 1.0 / vocab.size
      }
      else {
        0.0
      }
    }

    //Does not implement Linear
    var count_distance = 0
    var total_count_distance = 0
    //?? Finite state machines?
    //Used only here
    val hashmap_count_bar_end = new collection.mutable.HashMap[Int, Int]

    //Used to count all words
    val hashmap_count_words = new collection.mutable.HashMap[String, Int]

    // All variables first, methods second

    //Init Functions
    train(train)
    val total_end_bars:Double = this.hashmap_count_bar_end.foldLeft(0)((sum,t) => sum+t._2)

    val total_words:Double =    this.hashmap_count_words.foldLeft(0)((s,t) => s + t._2)

    def endbar_index_linear_probability(word: String, history: Seq[String]): Double = {
      var prob_any = 1.0 / vocab.size

      //Min is to prevent returning a number over 1
      //if alphas value gets too large
      val MAXIMUM_PROBABILITY = 1.0

      //****
      //Hacky way to make sure
      //'sample' method works as expected

      val bar_index = history.lastIndexOf("[BAR]")
      val end_bar_index = history.lastIndexOf("[/BAR]")

      val endbar_index_greater_than_bar_index = end_bar_index > bar_index

      if(bar_index != -1 && endbar_index_greater_than_bar_index) {
        count_distance = (history.size - 1) - end_bar_index
      } else {
        count_distance = 0
        curr_state = states("LINEAR")
        return bar_index_linear_probability(word, history)
      }
      //'sample' hacky
      //****

      var number_of_bars_at_index: Double = hashmap_count_bar_end.getOrElse(count_distance, 0).toDouble

      var prob_end_over_count: Double = number_of_bars_at_index / total_end_bars


      var prob_end_bar = Math.min(MAXIMUM_PROBABILITY,
        (prob_end_over_count) *alpha)

      if (vocab(word)) {
        //Change state
        if (word.contentEquals("[BAR]")) {
          curr_state = states("LINEAR")

          //Here return Uniform Probability
          return prob_end_bar
        }
        else {
          //this distance is not to change the prob of current word
          count_distance += 1

          //Max is to prevent returning a negative number
          // if alphas value gets too high
          val MINIMUM_PROBABILITY = 0.0
          return Math.max( MINIMUM_PROBABILITY , (1.0 - prob_end_bar)* prob_any )
        }

      }else {
        return 0.0
      }
    }

    def bar_index_linear_probability(word: String, history: Seq[String]): Double = {
      var prob_any = 1.0 / vocab.size

      //Min is to prevent returning a number over 1
      //if alphas value gets too large
      val MAXIMUM_PROBABILITY = 1.0

      //****
      //Hacky way to make sure
      //'sample' method works as expected

      val bar_index = history.lastIndexOf("[BAR]")
      val end_bar_index = history.lastIndexOf("[/BAR]")

      val bar_index_greater_than_end_bar_index = bar_index > end_bar_index

      if(bar_index != -1 && bar_index_greater_than_end_bar_index) {
        count_distance = (history.size - 1) - bar_index
      } else {
        count_distance = 0
        curr_state = states("END-LINEAR")
        return prob_any
      }
      //'sample' hacky
      //****

      var number_of_bars_at_index: Double = hashmap_count_bar_end.getOrElse(count_distance, 0).toDouble

      var prob_end_over_count: Double = number_of_bars_at_index / total_end_bars


      var prob_end_bar = Math.min(MAXIMUM_PROBABILITY,
        (prob_end_over_count) *alpha)

      if (vocab(word)) {
        //Change state
        if (word.contentEquals("[/BAR]")) {
          curr_state = states("END-LINEAR")

          //Here return Uniform Probability
          return prob_end_bar
        }
        else {
          //this distance is not to change the prob of current word
          count_distance += 1

          //Max is to prevent returning a negative number
          // if alphas value gets too high
          val MINIMUM_PROBABILITY = 0.0
          return Math.max( MINIMUM_PROBABILITY , (1.0 - prob_end_bar)* prob_any )
        }

      }else {
        return 0.0
      }
    }

    def unigram_probability(word: String, history: Seq[String]): Double = {
      if (vocab(word)) {
        return (hashmap_count_words.getOrElse(word,0).toDouble / total_words.toDouble)
      } else{
        return 0.0
      }
    }

    def train(train: IndexedSeq[String]): Unit =
    {
      //In this case we don't want order
      var can_count = false
      for (word <- train)
      {
        if(word == "[BAR]")
        {
          count_distance = 0
          can_count = true
        }
        else if(word == "[/BAR]") {
          // Count
          var map = hashmap_count_bar_end
          map(count_distance) = map.getOrElse(count_distance, 0) + 1
          total_count_distance += 1
          //Just incase we don't encounter a [\Bar]
          can_count = false
        }
        else if(can_count) {
          count_distance += 1
        }

        hashmap_count_words(word) = hashmap_count_words.getOrElse(word, 0) + 1

      }
    }

    def probability(word: String, history: String*): Double = {
      if (curr_state == states("UNIFORM")) {
        return uniform_probability(word, history)
      }
      else if (curr_state == states("LINEAR")) {
        return bar_index_linear_probability(word, history) * beta +
          unigram_probability(word, history) * (1 - beta)
      }
      else if (curr_state == states("END-LINEAR")) {
        return endbar_index_linear_probability(word, history) * beta +
          unigram_probability(word, history) * (1 - beta)
      }
      else {
        return 0.0
      }
    }

    //Call constructors:

  }

  def main(args: Array[String]) {
    //The training file we provide you
    val trainFile = new File(args(0))

    //the dev file we provide you.
    val devFile = new File(args(1))

    //the training sequence of words
    val train = Assignment1Util.loadWords(trainFile).toBuffer

    //the dev sequence of words
    val dev = Assignment1Util.loadWords(devFile).toBuffer

    //the vocabulary. Contains the training words and the OOV symbol (as the dev set has been preprocessed by
    //replacing words not in the training set with OOV).
    val vocab = train.toSet + Util.OOV

    //TODO: Improve the MyBarAwareLM implementation
    val lm = MyReallyGooDLM(train.toIndexedSeq, vocab)

    //This calculates the perplexity of the
    val pp = LanguageModel.perplexity(lm, dev)

    println(pp)

  }

}
