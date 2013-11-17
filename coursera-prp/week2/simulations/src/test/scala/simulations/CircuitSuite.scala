package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  val TRUE = true
  val FALSE = false

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(FALSE)
    in2.setSignal(FALSE)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(TRUE)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(TRUE)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(FALSE)
    in2.setSignal(FALSE)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(TRUE)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(TRUE)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(FALSE)
    in2.setSignal(FALSE)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(TRUE)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(TRUE)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("demux1 example") {
    val in = new Wire
    val c0 = new Wire
    val out0 = new Wire
    val out1 = new Wire
    demux(in, List(c0), List(out1, out0))

    c0.setSignal(FALSE)
    in.setSignal(FALSE)
    run
    assert(out0.getSignal === false, "demux1 1")
    assert(out1.getSignal === false, "demux1 2")

    in.setSignal(TRUE)
    run
    assert(out0.getSignal === true, "demux1 3")
    assert(out1.getSignal === false, "demux1 4")

    c0.setSignal(TRUE)
    run
    assert(out0.getSignal === false, "demux1 5")
    assert(out1.getSignal === true, "demux1 6")
  }
}
