
object sim extends Circuits with Parameters
import sim._
val in1, in2, sum, curry = new Wire
halfAdder(in1, in2, sum, curry)
probe("sum", sum)
probe("curry", curry)

in1 setSignal true
run()
in2 setSignal true
run()


in1 setSignal false
run()
