package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import scala.collection.mutable

/**
 * @author rockt
 */
object Main extends App {
  /**
   * Example training of a model
   *
   * Problems 2/3/4: perform a grid search over the parameters below
   */
  //Added by Skas
  var best_parameters = new mutable.MutableList[(Double,String)]()


  var learningRate = 0.01
  var vectorRegularizationStrength = 0.01

  val matrixRegularizationStrength = 0.0

  var wordDim = 10

  val hiddenDim = 10

  val trainSetName = "train"
  val validationSetName = "dev"
  
  var model: Model = new SumOfWordVectorsModel(wordDim, vectorRegularizationStrength)
  //val model: Model = new RecurrentNeuralNetworkModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)


  var x_epochs = new mutable.MutableList
  def epochHook(iter: Int, accLoss: Double): Unit = {
    val trainSetScore = 100 * Evaluator(model, trainSetName)
    val validSetScore = 100*Evaluator(model, validationSetName)

    var result_str = "Epoch %4d\tLoss %8.4f\tTrain Acc %4.2f\tDev Acc %4.2f".format(
      iter, accLoss, trainSetScore, validSetScore)

    result_str = result_str + s"\n\t wordDim: $wordDim -- learnRate: $learningRate -- regulariz: $vectorRegularizationStrength"
    println(result_str)


    best_parameters += ((validSetScore,result_str))
  }


  gridSearch(Seq(10,15,20),Seq(.001,  0.01, 0.1), Seq(0.001 ,0.01, 0.1) )
  //StochasticGradientDescentLearner(model, trainSetName, 6, learningRate, epochHook)

  def gridSearch(wordDims:Seq[Int], reguliser_params:Seq[Double] , learning_params:Seq[Double]): Unit = {
    for(dim <- wordDims) {
      wordDim = dim
      for(reg <- reguliser_params) {
        vectorRegularizationStrength = reg
        for(learning_rate <- learning_params ) {
          learningRate = learning_rate
          //overwrite
          model = new SumOfWordVectorsModel(wordDim, vectorRegularizationStrength)
          StochasticGradientDescentLearner(model, trainSetName, 6, learningRate, epochHook)

        }
      }
    }
    val sorted = best_parameters.sortBy(x => x._1)
    println("***********ENDED************")
    println("***********ENDED************")

    for(values <- sorted) {
      println(values)
    }

  }

  /**
   * Comment this in if you want to look at trained parameters
   */
  /*
  for ((paramName, paramBlock) <- model.vectorParams) {
    println(s"$paramName:\n${paramBlock.param}\n")
  }
  for ((paramName, paramBlock) <- model.matrixParams) {
    println(s"$paramName:\n${paramBlock.param}\n")
  }
  */
}