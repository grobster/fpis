package com.grobster.fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}
	
	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}
	
	def apply[A](as: A*): List[A] = if(as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
	
	def tail[A](li: List[A]): List[A] = li match {
		case Nil => Nil
		case Cons(_, t) => t
	}
	
	def setHead[A](value: A, li: List[A]): List[A] = li match {
		case Nil => Nil
		case Cons(h, t) => Cons(value, t)
	}
	
	def drop[A](l: List[A], n: Int): List[A] = n match {
		case 0 => Nil
		case _ => tail(l)
	}
	
}