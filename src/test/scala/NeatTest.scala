import neat.{Neat, Parameters}

object NeatTest extends App {
	implicit val params = Parameters()
	implicit val experiment = TestExperiment()
	val neat = Neat(100)
}
