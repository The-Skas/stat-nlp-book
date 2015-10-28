package uk.ac.ucl.cs.mr.statnlp2015.assignment1
import org.jfree.data.statistics.HistogramDataset
import java.io.File
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import org.sameersingh.scalaplot._
import uk.ac.ucl.cs.mr.statnlpbook.Segmenter
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{LanguageModel, UniformLM, Util}

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
    def some = Segmenter
    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*) = ???
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

    //This calculates the perplexity of the
    val pp = LanguageModel.perplexity(lm, dev)

    println(pp)

    //TODO:

    //TODO: combine a unigram model with the BAR aware LM through interpolation.
    //TODO: Improve the BarAwareLM to give probability 1 to a [BAR] following a [/BAR], and 0 otherwise.

  }
}
