package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }

    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    def or2Action() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal( !(!a1Sig & !a2Sig) ) }
    }
    a1 addAction or2Action
    a2 addAction or2Action
  }

  //TODO: Review demux implementation
  // Realizes a demultiplexer with n control wires (and 2 to the power n output wires).
  // The demultiplexer directs the signal from an input wire based on the control signal.
  // The rest of the output signals are set to 0. The demux method has the following signature:
  // The list of control wires (c) and the the list of output wires (out) are assumed to be sorted by decreasing index:
  // e.g. the head of the c list contains the control wire of index n-1.
  // Your implementation should be recursive and based on the gates that you implemented previously.
  // As a hint, think of the base case: the simplest demultiplexer has 0 control wires and 1 output wire.
  // Refer to the image below for some intuition on this last part.
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    (c, out) match {
      case (Nil, o :: Nil) => {
        val inv = new Wire
        inverter(in, inv)
        inverter(inv, o)
      }
      case (c :: Nil, List(o1, o0)) => {
        demuxHelper(in, c, o1, o0)
      }
      case (c1 :: cw, o) => {
        val w1, w0 = new Wire
        demuxHelper(in, c1, w1, w0)
        demux(w1, cw, o.take(o.length/2))
        demux(w0, cw, o.drop(o.length/2))
      }
      case default => println(s"$default cannot be handled")
    }

    def demuxHelper(in: Wire, c: Wire, o1: Wire, o0: Wire) {
      val inv = new Wire
      inverter(c, inv)
      andGate(in, inv, o0)
      andGate(in, c, o1)
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  val TRUE = true
  val FALSE = false

  def andGateExample() {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(FALSE)
    in2.setSignal(FALSE)
    run

    in1.setSignal(TRUE)
    run

    in2.setSignal(TRUE)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //

  def orGateExample() {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(FALSE)
    in2.setSignal(FALSE)
    run

    in1.setSignal(TRUE)
    run

    in2.setSignal(TRUE)
    run
  }

  def orGate2Example() {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(FALSE)
    in2.setSignal(FALSE)
    run

    in1.setSignal(TRUE)
    run

    in2.setSignal(TRUE)
    run
  }

  def demuxExample() {
    val in1 = new Wire
    val c0 = new Wire
    val out0 = new Wire
    val out1 = new Wire

    demux(in1, List(c0), List(out1, out0))
    probe("in1", in1)
    probe("c0", c0)
    probe("out0", out0)
    probe("out1", out1)

    c0.setSignal(FALSE)
    in1.setSignal(FALSE)
    run

    in1.setSignal(TRUE)
    run

    c0.setSignal(TRUE)
    run
  }

}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample()
  Circuit.orGateExample()
  Circuit.orGate2Example()
  Circuit.demuxExample()
}
