package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import uk.ac.ucl.cs.mr.statnlpbook.assignment2.Perceptron_Helpers._

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Created by Georgios on 11/11/2015.
 */
object Problem2 {


  /**
   * Train a linear model using the average perceptron algorithm.
   * @param instances the training instances.
   * @param feat a joint feature function.
   * @param predict a prediction function that maps inputs to outputs using the given weights.
   * @param iterations number of iterations.
   * @param learningRate
   * @tparam X type of input.
   * @tparam Y type of output.
   * @return a linear model trained using the perceptron algorithm.
   */
  def trainAvgPerceptron[X, Y](instances: Seq[(X, Y)],
                               feat: (X, Y) => FeatureVector,
                               predict: (X, Weights) => Y,
                               iterations: Int = 2,
                               learningRate: Double = 1.0): Weights = {
    //TODO implement the averaged perceptron trainer

    //TODO implement the perceptron trainer

    //First implement default 0 weight for all features.

    var weights:scala.collection.Map[FeatureKey, Double] = HashMap.empty[FeatureKey,Double].withDefaultValue(0.0)
    var weights_avg:scala.collection.Map[FeatureKey, Double] = HashMap.empty[FeatureKey,Double].withDefaultValue(0.0)
    feat.apply(instances(0)._1, instances(0)._2)

    var total_weight_count:Double = 0.0;

    //Initialized to one, so that we dont nullify vectory
    var current_weight_count:Double = 1.0;

    for((x,y) <- instances){
      var temp_predict = predict(x, weights)

      //IF Incorrect prediction.
      if(x.asInstanceOf[Candidate].gold != temp_predict){

        //Perceptron
        //Do something to the weights

        //φ(xi, cˆ)
        val feat_pred = feat.apply(x, temp_predict)

        // φ (xi, ci)
        val gold_pred = feat.apply(x, y)

        //weights_avg = weights_avg + count * weights
        weights_avg = add(weights_avg ,mult(weights, current_weight_count))
        //Reset weight_count
        current_weight_count = 0.0

        // weights ← weights + ( φ (xi, ci) − φ (xi, cˆ) )
        weights =  add(weights ,subtract(gold_pred, feat_pred))
      }
      current_weight_count += 1.0
      total_weight_count += 1.0
      //Look for case where gold != value
    }
    //feat(instances(0)) -> this should give us the FeatureVector

    //No need to otpimize iterations
    //Uses learning rate

    //use MutableWeight
    //Where is the gold value for the weights?

    var x = 0

    //Should return weights

    return mult(weights_avg, 1.0 / total_weight_count)

  }


  /**
   * Run this code to evaluate your implementation of your avereaged perceptron algorithm trainer
   * Results should be similar to the precompiled trainer
   * @param args
   */
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
    // no subsampling for dev/test!
    def getTriggerCandidates(docs: Seq[Document]) = docs.flatMap(_.triggerCandidates(0.02))
    def getTestTriggerCandidates(docs: Seq[Document]) = docs.flatMap(_.triggerCandidates())
    val triggerTrain = preprocess(getTriggerCandidates(trainDocs))
    val triggerDev = preprocess(getTestTriggerCandidates(devDocs))

    // get label set
    val triggerLabels = triggerTrain.map(_._2).toSet

    // define model
    val triggerModel = SimpleClassifier(triggerLabels, defaultTriggerFeatures)

    val myWeights = trainAvgPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
    val precompiledWeights = PrecompiledTrainers.trainAvgPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)

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

    //Label bias : Regulation, None, Phosphyralation or whatever y:Label passed in
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats.toMap
  }


}
