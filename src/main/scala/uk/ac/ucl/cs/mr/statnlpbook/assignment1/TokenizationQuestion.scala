package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlpbook.Tokenizer
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.Util

/**
 * This stub is provided as help for you to solve the first question.
 * @author riedel
 */
object TokenizationQuestion {

  def main(args: Array[String]) {

    //the directory that contains the raw documents
    val dataDir = new File(args(0))

    //the file that contains the target gold tokenization
    val goldFile = new File(args(1))

    //the actual raw documents
    val raw = Assignment1Util.loadTokenizationQuestionDocs(dataDir)

    //TODO: The tokenizer you need to improve to match the gold tokenization
    val tokenizer = Tokenizer.fromRegEx(???)

    //the result of the tokenization
    val result = Util.words(raw map tokenizer)

    //the gold tokenization file
    val gold = Assignment1Util.loadWords(goldFile).toBuffer

    //we find the first pair of tokens that don't match
    val mismatch = result.zip(gold).find { case (a, b) => a != b }

    //Your goal is to make sure that mismatch is None
    mismatch match {
      case None => println("Success!")
      case Some(pair) => println("The following tokens still don't match: " + pair)
    }

  }

}
