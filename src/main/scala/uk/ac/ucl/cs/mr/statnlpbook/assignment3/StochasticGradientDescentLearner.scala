package uk.ac.ucl.cs.mr.statnlpbook.assignment3

/**
 * Problem 2
 */
object StochasticGradientDescentLearner extends App {
  def apply(model: Model, corpus: String, maxEpochs: Int = 10, learningRate: Double, epochHook: (Int, Double) => Unit): Unit = {
    //Iteration divide by 3.0 to decrease time.
    val iterations = SentimentAnalysisCorpus.numExamples(corpus)
    for (i <- 0 until maxEpochs) {
      var accLoss = 0.0

      //modified TO RAW VALUE should be , iterations
      for (j <- 0 until iterations) {

        val (sentence, target) = SentimentAnalysisCorpus.getExample(corpus)
        //todo: update the parameters of the model and accumulate the loss
        val loss = model.loss(sentence, target)
        val score = loss.forward()
        if (score.isNaN()) {
            val wtf = "Oh shit"
        }


        var expected = 0
        if (target) expected = 1

        if (j % 1000 == 0) {
          val predicted_val = model.predict(sentence)
          print(s"Iter $j -- Score: $score -- Expected: $expected - Pred: $predicted_val \r")

        }
        loss.backward()
        loss.update(learningRate)   //Is the learning rate update correct?

        //Forward
        //Backward
        //Update using the gradient from backward
      }
      epochHook(i, accLoss)
    }
  }
}

