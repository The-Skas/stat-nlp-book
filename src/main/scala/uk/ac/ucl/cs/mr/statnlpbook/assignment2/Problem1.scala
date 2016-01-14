package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.Map
import uk.ac.ucl.cs.mr.statnlpbook.assignment2.Perceptron_Helpers._

/**
 * Created by Georgios on 11/11/2015.
 */
object Problem1 {

  /**
   * Train a linear model using the perceptron algorithm.
   * @param instances the training instances.
   * @param feat a joint feature function.
   * @param predict a prediction function that maps inputs to outputs using the given weights.
   * @param iterations number of iterations.
   * @param learningRate
   * @tparam X type of input.
   * @tparam Y type of output.
   * @return a linear model trained using the perceptron algorithm.
   */

  def trainPerceptron[X, Y](instances: Seq[(X, Y)],
                            feat: (X, Y) => FeatureVector,
                            predict: (X, Weights) => Y,
                            iterations: Int = 2,
                            learningRate:Double = 1.0): Weights = {
    //TODO implement the perceptron trainer

    //First implement default 0 weight for all features.

    var weights:scala.collection.Map[FeatureKey, Double] = HashMap.empty[FeatureKey,Double].withDefaultValue(0.0)

    feat.apply(instances(0)._1, instances(0)._2)


    for(label <- my_labels){
      println(label)
    }
    for((x,y) <- instances){
      println("key: "+x+ "-- Value: "+ y)
      println("cool")
      var temp_predict = predict(x, weights)

      //IF Incorrect prediction.
      if(x.asInstanceOf[Candidate].gold != temp_predict){

        //Perceptron
        //Do something to the weights

        //φ(xi, cˆ)
        val feat_pred = feat.apply(x, temp_predict)

        // φ (xi, ci)
        val gold_pred = feat.apply(x, y)

        // λ ← weights + ( φ (xi, ci) − φ (xi, cˆ) )
        weights = HashMap.empty[FeatureKey,Double].withDefaultValue(0.0) ++ add(weights ,subtract(gold_pred, feat_pred))

        println("Different: gold = "+ x + " -- value = "+temp_predict)


      }
      //Look for case where gold != value
    }
    //feat(instances(0)) -> this should give us the FeatureVector

    //No need to otpimize iterations
    //Uses learning rate

    //use MutableWeight
    //Where is the gold value for the weights?

    var x = 0
    println("Stop, Hammer Time!")

    //Should return weights

    return weights
  }


  /**
   * Run this code to evaluate your implementation of your perceptron algorithm trainer
   * Results should be similar to the precompiled trainer
   * @param args
   */

  var my_labels:Set[String] = null
  def main (args: Array[String] ) {

    val train_dir = "./data/assignment2/bionlp/train"

    // load train and dev data
    // read the specification of the method to load more/less data for debugging speedup
    val (trainDocs, devDocs) = BioNLP.getTrainDevDocuments(train_dir, 0.8, 100)
    // make tuples (Candidate,Gold)
    def preprocess(candidates: Seq[Candidate]) = candidates.map(e => e -> e.gold)

    // ================= Trigger Classification =================

    // get candidates and make tuples with gold
    // read the specifications of the method for different subsampling thresholds
    // no subsampling for dev/test! Skas: Here sub sampling is to reduce the time, so were only taking 2% of data
    def getTriggerCandidates(docs: Seq[Document]) = docs.flatMap(_.triggerCandidates(0.02))
    def getTestTriggerCandidates(docs: Seq[Document]) = docs.flatMap(_.triggerCandidates())
    val triggerTrain = preprocess(getTriggerCandidates(trainDocs))
    val triggerDev = preprocess(getTestTriggerCandidates(devDocs))

    // get label set
    val triggerLabels = triggerTrain.map(_._2).toSet

    //Skas adds this pixie dust
    my_labels = triggerTrain.map(_._2).toSet

    // define model
    val triggerModel = SimpleClassifier(triggerLabels, defaultTriggerFeatures)

    val myWeights = trainPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
    val precompiledWeights = PrecompiledTrainers.trainPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)

    // get predictions on dev
    val (myPred, gold) = triggerDev.map { case (trigger, gold) => (triggerModel.predict(trigger, myWeights), gold) }.unzip
    val (precompiledPred, _) = triggerDev.map { case (trigger, gold) => (triggerModel.predict(trigger, precompiledWeights), gold) }.unzip

    // evaluate models (dev)
    println("Evaluation - my trainer:")
    println(Evaluation(gold, myPred, Set("None")).toString)
    println("Evaluation - precompiled trainer:")
    println(Evaluation(gold, precompiledPred, Set("None")).toString)
  }

  def defaultTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats.toMap
  }


}
