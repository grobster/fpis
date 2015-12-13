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
	
	def drop[A](l: List[A], n: Int): List[A] = {
		def _drop(l: List[A], n: Int, nl: List[A]): List[A] = n match {
			case 0 => nl
			case _ => _drop(tail(l), n - 1, tail(l))
		}
		_drop(l, n, Nil)
	}
	
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Cons(h, t) if(f(h)) => dropWhile(t, f)
		case _ => l
	}
	
	def fill[A](n: Int, a: A): List[A] = {
		def _fill(n: Int, a: A, li: List[A]): List[A] = n match {
			case 0 => li
			case _ => _fill(n - 1, a, Cons(a, li))
		}
		_fill(n, a, Nil)
	}
	
	def init[A](li: List[A]): List[A] = {
		def _init(li: List[A], nl: List[A]): List[A] = li match {
			case Nil => nl
			case Cons(head, tail) if(tail != Nil) => _init(tail, Cons(head, nl))
			case Cons(head, tail) if(tail == Nil) => nl
		}
		_init(li, Nil)
    }
	
	def foldRight[A,B](li: List[A], z: B)(f: (A,B) => B): B = li match {
		case Nil => z
		case Cons(h, t) => f(h, foldRight(t, z)(f))
	}
	
	def sum2(li: List[Int]) = { foldRight(li, 0) { _ + _ } }
	
	def product2(li: List[Double]) = { foldRight(li, 1.0) (_ * _) }
	
	def length[A](li: List[A]): Int = foldRight(li, 0) { (_, acc) => acc + 1}
	
	def filter[A](li : List[A])(f: A => Boolean): List[A] = {
		def _filter(li: List[A], nl: List[A])(f: A => Boolean): List[A] = li match {
			case Nil => nl
			case Cons(h, t) if(f(h)) => _filter(t, Cons(h, nl))(f)
			case Cons(h, t) if(!f(h)) => _filter(t, nl)(f)
		}
		_filter(li, Nil)(f)
    }
	
	def map[A,B](li: List[A])(f: A => B): List[B] = li match {
		case Nil => Nil
		case Cons(h,t) => Cons(f(h), map(t)(f))
	}
	
	def reverse[A](li: List[A]): List[A] = foldLeft(li, List[A]()){ (b, a) => Cons(a, b)}
	
	def foldLeft[A,B](li: List[A], z: B)(f: (B,A) => B): B = li match {
		case Nil => z
		case Cons(h,t) => foldLeft(t, f(z, h))(f)
	}
}