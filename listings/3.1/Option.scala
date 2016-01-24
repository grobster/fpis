package com.grobster.fpinscala.option

trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(a) => Some(f(a))
	}
	
	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(a) => f(a)
	}
	
	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(a) => a
	}
	
	def orElse[B >: A](obj: => Option[B]): Option[B] = this match {
		case None => obj
		case _ => this
	}
	
	def filter(f: A => Boolean): Option[A] = this match {
		case Some(a) if(f(a)) => this
		case _ => None
	}
}

case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
	def mean(xs: Seq[Double]): Option[Double] = if(xs.isEmpty) None else Some(xs.sum / xs.length)
	
	def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
	
	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))
	
	def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
		case Nil => Some(Nil)
		case head :: tail => head flatMap(hh => sequence(tail) map (hh :: _))
	}
}