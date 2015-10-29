package uk.ac.ucl.cs.mr.statnlp2015.assignment1
import org.jfree.data.statistics.HistogramDataset
import java.io.File
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import org.sameersingh.scalaplot._
import uk.ac.ucl.cs.mr.statnlpbook.Segmenter
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{LanguageModel, UniformLM, Util}

import scala.collection.immutable.HashMap

/**
 * @author riedel
 */
object BarQuestion {

  /**
   * An LM that assigns increasing probability to the [/BAR] token the further the last [/BAR] token is away,
   * and uniform probability for all other tokens. How to choose the function that maps [/BAR] distance to a probability
   * is left to you, but you should guide your choice by the goal of minimizing perplexity.
   * @param vocab the vocabulary.
   */
  case class MyBarAwareLM(vocab: Set[String]) extends LanguageModel {
    def order = 20
    var alpha = 1
    def some = Segmenter

    val states: Map[String, Int] = Map("UNIFORM" -> 0
      , "LINEAR" -> 1);

    var curr_state:Int = states("UNIFORM")

    def uniform_probability(word: String, history: Seq[String]): Double =  {
      if (vocab(word)) {
        if(word.contentEquals("[BAR]")) {
          count_distance = 0
          curr_state = states("LINEAR")
        }
        return 1.0 / vocab.size
      }
      else {
        0.0
      }
    }

    //Does not implement Linear
    var count_distance = 0
    def linear_probability(word: String, history: Seq[String]): Double = {
      var prob_any = 1.0 / vocab.size

      //Min is to prevent returning a number over 1
      //if alphas value gets too large
      val MAXIMUM_PROBABILITY = 1.0
      var prob_end_bar = Math.min(MAXIMUM_PROBABILITY,
                                  prob_any * (count_distance + 1) *alpha)

      if (vocab(word)) {
        //Change state
        if (word.contentEquals("[/BAR]")) {
          curr_state = states("UNIFORM")

          //Here return Uniform Probability
          return prob_end_bar
        }
        else {
          //Assert

          count_distance += 1
          //Max is to prevent returning a negative number
          // if alphas value gets too high
          val MINIMUM_PROBABILITY = 0.0
          return Math.max( MINIMUM_PROBABILITY , 1.0 - prob_end_bar)
        }

      }else {
        return 0.0
      }
    }

    //?? Finite state machines?

    def probability(word: String, history: String*): Double = {
      if (curr_state == states("UNIFORM")) {
        return uniform_probability(word, history)
      }
      else if (curr_state == states("LINEAR")) {
        return linear_probability(word, history)
      }
      else {
        return 0.0
      }
    }

  }

  case class UniformProbLM(vocab: Set[String]) extends LanguageModel {
    def order = 20
    def some = Segmenter
    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*) =
      if (vocab(word)) 1.0 / vocab.size else 0.0
  }

  def main(args: Array[String]) {


    //The training file we provide you
    val trainFile = new File("data/p2/p2_train.txt")

    //the dev file we provide you.
    val devFile = new File("data/p2/p2_dev.txt")

    //the training sequence of words
    val train = Assignment1Util.loadWords(trainFile).toBuffer



    //the dev sequence of words
    val dev = Assignment1Util.loadWords(devFile).toBuffer

    //the vocabulary. Contains the training words and the OOV symbol (as the dev set has been preprocessed by
    //replacing words not in the training set with OOV).
    val vocab = train.toSet + Util.OOV

    //TODO: Improve the MyBarAwareLM implementation
    val lm = MyBarAwareLM(vocab)

    val alpha_values = List.range(1, 300, 10)
    //This calculates the perplexity of the
    for( i <- 0 until alpha_values.size) {
      lm.alpha = alpha_values(i)
      var pp = LanguageModel.perplexity(lm, dev)
      println("alpha: "+alpha_values(i)+" -- Perplexity: "+pp)
    }


    //TODO:

    //TODO: combine a unigram model with the BAR aware LM through interpolation.
    //TODO: Improve the BarAwareLM to give probability 1 to a [BAR] following a [/BAR], and 0 otherwise.

  }
}
