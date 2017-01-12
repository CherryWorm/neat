import neat.Parameters

import scala.language.implicitConversions
import util._


case class FunctionTuple[A, B](f: (A, B) => (A, B)) {
	def comp(o: FunctionTuple[A, B]): FunctionTuple[A, B] = (a: A, b: B) => this(o(a, b))
	def comp(f: (A, B) => (A, B)): FunctionTuple[A, B] = comp(FunctionTuple(f))
	def apply(t: (A,B)) = f.tupled(t)
	def apply(a: A, b: B) = f(a, b)
}

package object util {
	def iter[A] (n: Int, a: A)(f: A => A): A = if (n == 0) a else iter (n-1, f(a)) (f)
	def maybe[A](b: => Boolean, f: A => A)(a: A): A = if(b) f(a) else a
	def maybeP[A](prob: Double, f: A => A)(implicit parameters: Parameters) = maybe(p(prob), f) _
	def p(prob: Double)(implicit parameters: Parameters): Boolean = prob < parameters.random.nextDouble()
	def rand[A](l: List[A])(implicit parameters: Parameters): A = l(parameters.random.nextInt(l.length))
	
	implicit def function2functionTuple[A, B](f: (A, B) => (A, B)): FunctionTuple[A, B] = FunctionTuple(f)
}
