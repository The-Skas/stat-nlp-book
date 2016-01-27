package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import breeze.numerics.{log, sigmoid, tanh} //yes, you will need them ;)
import breeze.linalg.DenseMatrix

/**
 * @author rockt
 */

/**
 * A trait for the core building **block** of our computation graphs
 * @tparam T the type parameter this block evaluates to (can be Double, Vector, Matrix)
 */
trait Block[T] {
  //caches output after call of forward
  var output: T = _
  //fun fact: when you say "forward" or "back" your lips move in the respective direction
  def forward(): T
  //assumes that forward has been called first!
  def backward(gradient: T): Unit
  //updates parameters of the block
  def update(learningRate: Double)
}

/**
 * A loss function is a block that evaluates to a single double.
 * Generally loss functions don't have upstream gradients,
 * so we can provide an implementation of backward without arguments.
 */
trait Loss extends Block[Double] {
  def backward(): Unit
}

trait ParamBlock[P] extends Block[P] {
  var param: P
  val gradParam: P
  def initialize(dist: () => Double): P
  def set(p: P): Unit = {
    param = p
  }
  def resetGradient(): Unit
}

trait DefaultInitialization {
  def defaultInitialization(): Double
}

/**
 * This trait defines a default way of initializing weights of parameters
 */
trait GaussianDefaultInitialization extends DefaultInitialization {
  def defaultInitialization(): Double = random.nextGaussian() * 0.1
}

trait VectorDerive {
  def deriveVector(vec: Vector): Unit = {

  }
}
/**
 * A simple block that represents a constant double value
 * @param arg the constant double value
 */
case class DoubleConstant(arg: Double) extends Block[Double] with Loss {
  output = arg
  def forward(): Double = output
  def backward(gradient: Double): Unit = {} //nothing to do since fixed
  def update(learningRate: Double): Unit = {} //nothing to do since fixed and no child blocks
  def backward(): Unit = {} //nothing to do since fixed
}

/**
 * A simple block that represents a constant vector
 * @param arg the constant vector
 */
case class VectorConstant(arg: Vector) extends Block[Vector] {
  output = arg
  def forward(): Vector = output
  def backward(gradient: Vector): Unit = {} //nothing to do since fixed
  def update(learningRate: Double): Unit = {} //nothing to do since fixed and no child blocks
}

/**
 * A block representing a sum of doubles
 * @param args a sequence of blocks that evaluate to doubles
 */
case class DoubleSum(args: Block[Double]*) extends Block[Double] {
  def forward(): Double = {
    //_.forward() -- returns the acctual double value
    output = args.map(_.forward()).sum
    output
  }
  def backward(gradient: Double): Unit = args.foreach(_.backward(gradient))
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
}

class LossSum(override val args: Loss*) extends DoubleSum(args:_*) with Loss {
  def backward(): Unit = args.foreach(_.backward())
}



/**
 * Problem 2
 */


/**
 * A block representing a vector parameter
 * @param dim dimension of the vector
 * @param clip defines range in which gradients are clipped, i.e., (-clip, clip)
 */
case class VectorParam(dim: Int, clip: Double = 10.0) extends ParamBlock[Vector] with GaussianDefaultInitialization {
  var param: Vector = initialize(() => defaultInitialization())
  val gradParam: Vector = randVec(dim, () => 0) //todo: initialize with zeros
  /**
   * @return the current value of the vector parameter and caches it into output
   */
  def forward(): Vector = {

    output = param
    output.copy
  }
  /**
   * Accumulates the gradient in gradParam
   * @param gradient an upstream gradient
   */
  def backward(gradient: Vector): Unit = {
    //Shouldnt it pass all the gradient into all inputs
    if(IsNaN(gradient)){
      val x = "Break_me"
    }
    gradParam += gradient

  }
  /**
   * Resets gradParam to zero
   */
  def resetGradient(): Unit = {
    for(i <- 0 until gradParam.activeSize) {
      gradParam(i) = 0.0
    }
  }
  /**
   * Updates param using the accumulated gradient. Clips the gradient to the interval (-clip, clip) before the update
   * @param learningRate learning rate used for the update
   */
  def update(learningRate: Double): Unit = {
    val old_param = param.copy
    param :-= (breeze.linalg.clip(gradParam, -clip, clip) * learningRate) //in-place

    if(IsNaN(param)){
      val x = "Break_me"
    }

    resetGradient()
  }

  def IsNaN(a_vec:Vector): Boolean ={
    for(i <- 0 until a_vec.activeSize) {
      if(a_vec(i).isNaN()){
        return true
      }
    }
    return false
  }
  /**
   * Initializes the parameter randomly using a sampling function
   * @param dist sampling function
   * @return the random parameter vector
   */
  def initialize(dist: () => Double): Vector = {
    param = randVec(dim, dist)
    param
  }
}


//MY Class vector sum
case class MySum(vec: ParamBlock[Vector]) extends Block[Double] {
  def forward(): Double = {
    //Could be wrong code, I mean what should forward return?

    //todo: make sure formula is correct
    output = vec.forward().sum
    output
  }
  def backward(gradient: Double): Unit = {

    vec.backward(breeze.linalg.DenseVector.fill(vec.gradParam.activeSize){
      gradient
    })
  }
  def update(learningRate: Double): Unit = {
    vec.update(learningRate)
  }
}



/**
 * A block representing the sum of vectors
 * @param args a sequence of blocks that evaluate to vectors
 */
case class Sum(args: Seq[Block[Vector]]) extends Block[Vector] {
  def forward(): Vector = {
    //Could be wrong code, I mean what should forward return?
    var endVector = args(0).forward()
    for(i <- 1 until args.size){
      endVector :+= args(i).forward()
    }
    //todo: make sure formula is correct
    output = endVector
    output
  }
  def backward(gradient: Vector): Unit = {
    args.foreach(_.backward(gradient))
  }
  def update(learningRate: Double): Unit = {
    args.foreach(_.update(learningRate))
  }
}

/**
 * A block representing the dot product between two vectors
 * @param arg1 left block that evaluates to a vector
 * @param arg2 right block that evaluates to a vector
 */
case class Dot(arg1: Block[Vector], arg2: Block[Vector]) extends Block[Double] {
  def dotProduct(vec1:Vector, vec2:Vector): Double = {
    var dot_product = 0.0

    for(i <- 0 until vec1.activeSize){
      dot_product += vec1(i) * vec2(i)
    }
    return dot_product
  }

  def forward(): Double = {
    return dotProduct(arg1.forward(), arg2.forward())
  }

  def backward(gradient: Double): Unit = {
    val vec1= arg1.forward()
    //Vec1 differentiate product
    val vec2 = arg2.forward()

    vec1 :*= gradient

    vec2 :*= gradient



    //Calculates the differential
    arg1.backward(vec2)

    arg2.backward(vec1)
  }
  def update(learningRate: Double): Unit = {
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}

/**
 * A block representing the sigmoid of a scalar value
 * @param arg a block that evaluates to a double
 */
case class Sigmoid(arg: Block[Double]) extends Block[Double] {
  def forward(): Double = {
    breeze.numerics.sigmoid(arg.forward())
  }
  def backward(gradient: Double): Unit = {
    //s(x)(1-(s(x))
    val pos_sig = breeze.numerics.sigmoid(arg.forward())
    val neg_sig = 1.0 - breeze.numerics.sigmoid(arg.forward())
    val deriv_sig = pos_sig * neg_sig

    arg.backward(deriv_sig * gradient)
  }
  def update(learningRate: Double): Unit = {
    arg.update(learningRate)
  }
}

/**
 * A block representing the negative log-likelihood loss
 * @param arg a block evaluating to a scalar value
 * @param target the target value (1.0 positive sentiment, 0.0 negative sentiment)
 */
case class NegativeLogLikelihoodLoss(arg: Block[Double], target: Double) extends Loss {
  def forward(): Double = {
    val y = target

    var arg_val = arg.forward()

    val left_operation = -y * math.log10( arg_val)


    if(arg_val >= 1.0){
      arg_val = 1.0 - 1e-8
    }
    val right_operation = (1.0 - y) * log( 1.0 - arg_val)

    left_operation - right_operation
  }
  //loss functions are root nodes so they don't have upstream gradients
  def backward(gradient: Double): Unit = backward()
  def backward(): Unit = {
    val e = math.E
    val y = target
    val ln_10 = math.log10(10) / math.log10(e)


    var left_operation =       -y * (1.0 / (ln_10 * (arg.forward())))


    var right_operation = (1.0-y) * (1.0 / (ln_10 * (arg.forward() - 1.0)))

    if(left_operation.isNaN()  || left_operation.isInfinity){
      left_operation = 0.0
    }

    if(right_operation.isNaN() || right_operation.isInfinity ){
      right_operation = 0.0
    }




    arg.backward(left_operation - right_operation)
  }
  def update(learningRate: Double): Unit = {
    arg.update(learningRate)
  }
}

/**
 * A block representing the l2 regularization of a vector or matrix
 * @param strength the strength of the regularization (often denoted as lambda)
 * @param args a block evaluating to a vector or matrix
 * @tparam P type of the input block (we assume this is Block[Vector] or Block[Matrix]
 */
case class L2Regularization[P](strength: Double, args: Block[P]*) extends Loss {
  def forward(): Double = {
    /**
     * Calculates the loss individually for every vector/matrix parameter in args
     */
    var total_reg:Double = 0.0

    val losses = args.map(arg => {
      val in = arg.forward()
      in match {
        case v: Vector => {
          var norm: Double = 0.0
          for (i <- 0 until v.activeSize) {
            norm += v(i) * v(i)
          }
          total_reg += (strength / 2.0) * norm
        }
        case w: Matrix => {
          var norm: Double = 0.0
          for (i <- 0 until w.rows) {
            for(j <-0 until w.cols) {
              norm += w(i,j) * w(i,j)
            }
          }
          total_reg += (strength / 2.0) * norm
        }
      }
    })
    output = total_reg //sums the losses up
    output
  }
  def update(learningRate: Double): Unit = {
    args.map(arg => {
      //Is this correct?
      arg.update(learningRate)
    })
  }
  //loss functions are root nodes so they don't have upstream gradients
  def backward(gradient: Double): Unit = backward()
  def backward(): Unit = args.foreach(x => x.backward((x.forward() match {
    case v: Vector => {
      var deriv_norm: Double = 0.0
      for (i <- 0 until v.activeSize) {
        v(i) = (strength / 2.0) * v(i) * 2.0
      }
      v //will this return v
    }
    case w: Matrix => {
      var deriv_norm: Double = 0.0
      for (i <- 0 until w.rows) {
        for(j <-0 until w.cols) {
          w(i,j) = (strength / 2.0) * w(i,j) * 2.0
        }
      }
      w
    }
    // will return w
  }).asInstanceOf[P]))
}



/**
 * Problem 3
 */

/**
 * A block representing a matrix parameter
 * @param dim1 first dimension of the matrix
 * @param dim2 second dimension of the matrix
 * @param clip defines range in which gradients are clipped, i.e., (-clip, clip)
 */
case class MatrixParam(dim1: Int, dim2: Int, clip: Double = 10.0) extends ParamBlock[Matrix] with GaussianDefaultInitialization {
  var param: Matrix = ???
  val gradParam: Matrix = ???
  def forward(): Matrix = ???
  def backward(gradient: Matrix): Unit = ???
  def resetGradient(): Unit = ???
  def update(learningRate: Double): Unit = ???
  def initialize(dist: () => Double): Matrix = {
    param = randMat(dim1, dim2, dist)
    param
  }
}

/**
 * A block representing matrix-vector multiplication
 * @param arg1 the left block evaluating to a matrix
 * @param arg2 the right block evaluation to a vector
 */
case class Mul(arg1: Block[Matrix], arg2: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = ???
  def backward(gradient: Vector): Unit = ???
  def update(learningRate: Double): Unit = ???
}

/**
 * A block rerpesenting the element-wise application of the tanh function to a vector
 * @param arg a block evaluating to a vector
 */
case class Tanh(arg: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = ???
  def backward(gradient: Vector): Unit = ???
  def update(learningRate: Double): Unit = ???
}



/**
  * Problem 4
  */

/**
 * A potentially useful block for training a better model (https://en.wikipedia.org/wiki/Dropout_(neural_networks))
 * @param prob dropout probability
 * @param arg a block evaluating to a vector whose components we want to drop
 */
case class Dropout(prob: Double, arg: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = ???
  def update(learningRate: Double): Unit = ???
  def backward(gradient: Vector): Unit = ???
}

/**
  * ... be free, be creative :)
  */