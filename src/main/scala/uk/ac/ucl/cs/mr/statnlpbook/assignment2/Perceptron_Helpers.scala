package uk.ac.ucl.cs.mr.statnlpbook.assignment2


import scala.collection.Map
import scala.collection.mutable.HashMap

/**
 * Created by skas on 12/17/15.
 */
object Perceptron_Helpers {
  def add(featA: Map[FeatureKey, Double],
          featB: Map[FeatureKey, Double]): Map[FeatureKey, Double] = {
    var mutable_A = collection.mutable.Map(featA.toSeq: _*)

    for((k,v) <- featB) {

      var key_val:Double = featA.getOrElse(k, 0)

      mutable_A(k) = key_val + v
    }
    return HashMap.empty[FeatureKey,Double].withDefaultValue(0.0) ++ mutable_A
  }

  //ORDER: A - B
  def subtract(featA: Map[FeatureKey, Double],
               featB: Map[FeatureKey, Double]): Map[FeatureKey, Double] = {
    var mutable_A = collection.mutable.Map(featA.toSeq: _*)

    for((k,v) <- featB) {
      var key_val:Double = featA.getOrElse(k, 0)

      mutable_A(k) = key_val - v
    }
    return HashMap.empty[FeatureKey,Double].withDefaultValue(0.0) ++ mutable_A
  }

  def mult(featA: Map[FeatureKey, Double],
          scale: Double): Map[FeatureKey, Double] = {

    var mutable_A = collection.mutable.Map(featA.toSeq: _*)

    for((k,v) <- featA) {

      //var key_val:Double = featA.getOrElse(k, 0)

      mutable_A(k) = v * scale
    }
    return HashMap.empty[FeatureKey,Double].withDefaultValue(0.0) ++ mutable_A
  }
}
