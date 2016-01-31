package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import breeze.linalg.{QuasiTensor, TensorLike, sum}
import breeze.numerics._

/**
 * Problem 1
 */
object GradientChecker extends App {
  val EPSILON = 1e-6

  /**
   * For an introduction see http://cs231n.github.io/neural-networks-3/#gradcheck
   *
   * This is a basic implementation of gradient checking.
   * It is restricted in that it assumes that the function to test evaluates to a double.
   * Moreover, another constraint is that it always tests by backpropagating a gradient of 1.0.
   */
  def apply[P](model: Block[Double], paramBlock: ParamBlock[P]) = {
    paramBlock.resetGradient()
    model.forward()
    model.backward(1.0)

    var avgError = 0.0

    val gradient = paramBlock.gradParam match {
      case m: Matrix => m.toDenseVector
      case v: Vector => v
    }

    /**
     * Calculates f_theta(x_i + eps)
     * @param index i in x_i
     * @param eps value that is added to x_i
     * @return
     */
    def wiggledForward(index: Int, eps: Double): Double = {
      var result = 0.0
      paramBlock.param match {
        case v: Vector =>
          val tmp = v(index)
          v(index) = tmp + eps
          //Get model result
          result = model.forward()
          v(index) = tmp
        case m: Matrix =>
          val (row, col) = m.rowColumnFromLinearIndex(index)

          val tmp = m(row, col)
          m(row, col) = tmp + eps
          result = model.forward()
          m(row, col) = tmp
      }
      result
    }

    for (i <- 0 until gradient.activeSize) {
      //∂gθ    gθ(x + e ) − gθ(x − e)
      //--- ≈  ---------------------
      //∂xi           2 ∗ e



      val plus_epsilon = wiggledForward(i, EPSILON)
      val min_epsilon  = wiggledForward(i, -EPSILON)
      val gradientApprox = (plus_epsilon - min_epsilon) / (2.0 * EPSILON)

      val gradientExpected: Double = gradientApprox

      avgError = avgError + math.abs(gradientExpected - gradient(i))

      assert(
        math.abs(gradientExpected - gradient(i)) < EPSILON,
        "Gradient check failed!\n" +
          s"Expected gradient for ${i}th component in input is $gradientExpected but I got ${gradient(i)}"
      )
    }

    println("Average error: " + avgError)

  }

  /**
    * A very silly block to test if gradient checking is working.
    * Will only work if the implementation of the Dot block is already correct
    */




  val a = VectorParam(3)
  val b = VectorParam(3)
  a.set(vec(-2.0,4.0, 3.0))
  b.set(vec(5.0,3.0, 2.0))

  val sigmoidBlock = Dot(b, vec(1.0,1.0,1.0))
  println("Sigmoid Forward 5.0 = ")
  GradientChecker(sigmoidBlock, b)

  val simpleBlock = Dot(a, b)
  println("Dot Product: "+ simpleBlock.forward())
  println("Dot Gradient: ")
 // GradientChecker(simpleBlock, b)
 // GradientChecker(simpleBlock, a)

  //Cant do it for this.
  val sigmoid = Sigmoid(Dot(a, b))
  println("Sigmoid: "+sigmoid.forward())
  //println("Sum Gradient: ")
  GradientChecker(sigmoid, b)


  val neglogBlock = NegativeLogLikelihoodLoss(Sigmoid(Dot(a, b)), 1.0)
  println("Negative Log like Gradient")
  GradientChecker(neglogBlock, b)


  //L2Regularization
  val regBlock = L2Regularization(1.0, b)
  println("L2 Reg Gradient")


  //Matrix checker:
  println("Matrix TanhGradient:")
  var matrxParam = MatrixParam(3,3)
  matrxParam.set(mat(3,3)(1,2,3,4,5,6,7,8,9))
  val tanhBlock = Dot(a,Tanh( b))
  GradientChecker(tanhBlock, b)

//  println("Matrix multi gradient")
//  matrxParam.set(mat(3,3)(1,2,3,4,5,6,7,8,9))
//  val multBlock = Dot(a,(Mul(matrxParam, b)))
//  GradientChecker(multBlock, matrxParam)

  println("Matrix multi gradient")
  matrxParam.set(mat(3,3)(1,0,0,0,1,0,0,0,1))
  val multBlock2 = Dot(a,(Mul(matrxParam, b)))
  GradientChecker(multBlock2, matrxParam)









}
