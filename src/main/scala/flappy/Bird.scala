package flappy

import neat.NeuralNetwork

import scala.collection.mutable
import scala.util.Random

sealed trait Object

class Bird(var height: Double) extends Object {
	var dy: Double = 0
	val size = 1
	
	def update(d: Double) = {
		dy -= d * 9.81
		height -= d * dy
	}
	
	def collidesWith(o: Object) = o match {
		case Ground(y) => height - 0.5 < y
		case Pipe(lower, x) if x < 1 && x > -1 => height < lower || height > lower + 2
		case _ => false
	}
}

case class Ground(y: Int) extends Object

case class Pipe(lower: Double, var x: Double) extends Object {
	def update(d: Double, speed: Double) = {
		x -= d * speed
	}
	
}

class Game {
	
	val ground = Ground(0)
	val pipes = mutable.Queue[Pipe]()
	val random = new Random()
	for(i <- 2 to 4) {
		pipes.enqueue(Pipe(random.nextDouble() * 6 + 2, 5 * i))
	}
	val bird = new Bird(4)
	var speed = 3.0
	
	def eval(net: NeuralNetwork): Double = {
		var score = 0.0
		while(step(0.03)) {
			score += (speed - 0.05) * 0.03
			val buffer = mutable.Buffer[Double]()
			buffer += bird.height
			buffer += bird.dy
			buffer += speed
			for(i <- 0 until (50 * 50 - 3))
				buffer += 0
			val res = net.step(buffer)
			if(res.head > 0)
				bird.dy = 2
		}
		score
	}
	
	def step(d: Double): Boolean = {
		bird.update(d)
		pipes.foreach(_.update(d, speed))
		
		val p = pipes.front
		if(pipes.front.x < -20) {
			pipes.dequeue()
			pipes.enqueue(Pipe(random.nextDouble() * 6 + 2, pipes.front.x + 40))
		}
		
		speed += d * 0.05
		
		if(pipes exists (bird.collidesWith(_)))
			false
		else if(bird.collidesWith(ground))
			false
		else
			true
	}
	
}