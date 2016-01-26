package uk.ac.ucl.cs.mr.statnlpbook.assignment3

/**
 * Problem 2
 */
object StochasticGradientDescentLearner extends App {
  def apply(model: Model, corpus: String, maxEpochs: Int = 10, learningRate: Double, epochHook: (Int, Double) => Unit): Unit = {
    //Iteration divide by 3.0 to decrease time.
    val iterations = (SentimentAnalysisCorpus.numExamples(corpus).toDouble / 3.0).toInt
    for (i <- 0 until maxEpochs) {
      var accLoss = 0.0

      //modified TO RAW VALUE should be , iteration
      for (j <- 0 until 30000) {

        val (sentence, target) = SentimentAnalysisCorpus.getExample(corpus)
        //todo: update the parameters of the model and accumulate the loss
        val loss = model.loss(sentence, target)
        val score = loss.forward()

        var expected = 0
        if (target) expected = 1

        if (j % 1000 == 0) print(s"Iter $j -- Score: $score -- Expected: $expected\r")
        loss.backward()
        loss.update(learningRate)   //Is the learning rate update correct?
        val afafax = 0
        //for(word <- sentence) {
          //model.wordToVector(word).update
        //}
        //Forward
        //Backward
        //Update using the gradient from backward
      }
      epochHook(i, accLoss)
    }
  }
}

