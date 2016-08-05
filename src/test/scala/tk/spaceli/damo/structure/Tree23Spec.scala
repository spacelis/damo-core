package tk.spaceli.damo.structure

/**
  * Created by ucfawli on 05-Aug-16.
  */


import org.scalatest._
import org.scalatest.prop.PropertyChecks

import scala.math.Ordering.Int

class Tree23Spec extends FlatSpec with Matchers with PropertyChecks {

  "Empty Tree" should "contain nothing" in {
    val e = Empty[Int]()
    e search 1 should be (None)
  }

  "A Tree" should "find the just inserted" in {
    val t = Tree23Root[Int](Empty())
    forAll {n: Int =>
      val t2 = t insert n
      t2 search n should be (Some(n))
    }
  }
}
