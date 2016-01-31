package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import scala.collection.mutable

/**
 * @author rockt
 */
trait Model {
  /**
   * Stores all vector parameters
   */
  val vectorParams = new mutable.HashMap[String, VectorParam]()
  /**
   * Stores all matrix parameters
   */
  val matrixParams = new mutable.HashMap[String, MatrixParam]()
  /**
   * Maps a word to its trainable or fixed vector representation
   * @param word the input word represented as string
   * @return a block that evaluates to a vector/embedding for that word
   */
  def wordToVector(word: String): Block[Vector]
  /**
   * Composes a sequence of word vectors to a sentence vectors
   * @param words a sequence of blocks that evaluate to word vectors
   * @return a block evaluating to a sentence vector
   */
  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector]
  /**
   * Calculates the score of a sentence based on the vector representation of that sentence
   * @param sentence a block evaluating to a sentence vector
   * @return a block evaluating to the score between 0.0 and 1.0 of that sentence (1.0 positive sentiment, 0.0 negative sentiment)
   */
  def scoreSentence(sentence: Block[Vector]): Block[Double]
  /**
   * Predicts whether a sentence is of positive or negative sentiment (true: positive, false: negative)
   * @param sentence a tweet as a sequence of words
   * @param threshold the value above which we predict positive sentiment
   * @return whether the sentence is of positive sentiment
   */
  def predict(sentence: Seq[String])(implicit threshold: Double = 0.5): Boolean = {
    val wordVectors = sentence.map(wordToVector)
    val sentenceVector = wordVectorsToSentenceVector(wordVectors)
    scoreSentence(sentenceVector).forward() >= threshold
  }
  /**
   * Defines the training loss
   * @param sentence a tweet as a sequence of words
   * @param target the gold label of the tweet (true: positive sentiement, false: negative sentiment)
   * @return a block evaluating to the negative log-likelihod plus a regularization term
   */
  def loss(sentence: Seq[String], target: Boolean): Loss = {
    val targetScore = if (target) 1.0 else 0.0
    val wordVectors = sentence.map(wordToVector)
    val sentenceVector = wordVectorsToSentenceVector(wordVectors)
    val score = scoreSentence(sentenceVector)
    new LossSum(NegativeLogLikelihoodLoss(score, targetScore), regularizer(wordVectors))
  }
  /**
   * Regularizes the parameters of the model for a given input example
   * @param words a sequence of blocks evaluating to word vectors
   * @return a block representing the regularization loss on the parameters of the model
   */
  def regularizer(words: Seq[Block[Vector]]): Loss
}


/**
 * Problem 2
 * A sum of word vectors model
 * @param embeddingSize dimension of the word vectors used in this model
 * @param regularizationStrength strength of the regularization on the word vectors and global parameter vector w
 */
class SumOfWordVectorsModel(embeddingSize: Int, regularizationStrength: Double = 0.0) extends Model {
  /**
   * We use a lookup table to keep track of the word representations
   */
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  /**
   * We are also going to need another global vector parameter
   */

  //param_w is the weight parameter, it should be the weight for each
  //word found. Issue is.. How would I be able to get the weight of a word?

  //So this should be defaulted to 1.0 for each parameter...
  //
  val WEIGHT_STR = "param_w"
  LookupTable.addTrainableWordVector(WEIGHT_STR, embeddingSize)

  def wordToVector(word: String): Block[Vector] = {
    //word
    return LookupTable.addTrainableWordVector(word , embeddingSize)
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {

//    for( i <- 0 until words.length){
//      output.param :+= words(i).forward()
//    }

    return Sum(words)
    //return VectorConstant(output.param)
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = {
    return Sigmoid(Dot(sentence, LookupTable.get(WEIGHT_STR)))
  }

  def regularizer(words: Seq[Block[Vector]]): Loss = L2Regularization(regularizationStrength, words:_*)
}


/**
 * Problem 3
 * A recurrent neural network model
 * @param embeddingSize dimension of the word vectors used in this model
 * @param hiddenSize dimension of the hidden state vector used in this model
 * @param vectorRegularizationStrength strength of the regularization on the word vectors and global parameter vector w
 * @param matrixRegularizationStrength strength of the regularization of the transition matrices used in this model
 */
class RecurrentNeuralNetworkModel(embeddingSize: Int, hiddenSize: Int,
                                  vectorRegularizationStrength: Double = 0.0,
                                  matrixRegularizationStrength: Double = 0.0) extends Model {
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  //W for regularization?
  vectorParams += "param_w" -> VectorParam(embeddingSize)

  vectorParams += "param_h0" -> VectorParam(hiddenSize)
  vectorParams += "param_b" -> VectorParam(hiddenSize)

  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()
  matrixParams += "param_Wx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wh" -> MatrixParam(hiddenSize, hiddenSize)

  val STR_WX = "param_Wx"
  val STR_WH = "param_Wh"
  val STR_B  = "param_b"
  val STR_H0 = "param_h0"
  val WEIGHT_STR = "param_w"
  def wordToVector(word: String): Block[Vector] = {
    return LookupTable.addTrainableWordVector(word , embeddingSize)

  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
    Sum(words)
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = {
    //tanh([W^h]*h_t−1 + (W^x)x_t + b)
    var wx:MatrixParam = null
    var wh:MatrixParam = null

    var b:Block[Vector] = LookupTable.get(STR_B)
    var h0:Block[Vector] = LookupTable.get(STR_H0)
    var w:Block[Vector] = LookupTable.get(WEIGHT_STR)

    matrixParams.get(STR_WX) match {
      case x: MatrixParam => wx = x
    }

    matrixParams.get(STR_WH) match {
      case x: MatrixParam => wh = x
    }

    //tanh([W^h]*h_t−1 + (W^x)x_t + b)
    val mult_1 = Mul(wh, h0)
    val mult_2 = Mul(wx, sentence)

    val sum_block = Sum(Seq(mult_1, mult_2, b))

    return Tanh(sum_block)
   // Mul(wx, LookupTable.get(STR_H0))
    //Tanh(Sum())
    ???
  }

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, words:_*),
      L2Regularization(matrixRegularizationStrength, words:_*)
    )
}