package com.grobster.fpinscala.stream

sealed trait Stream[+A] {
	def toList: List[A] = {
		def go(s: Stream[A], acc: List[A]): List[A] = s match {
			case Cons(h,t) => go(t(), h() :: acc)
			case _ => acc
		}
		go(this, List()).reverse
	}
	
	def take(n: Int): Stream[A] = this match {
		case Cons(h,t) if(n > 1) => Stream.cons(h(), t().take(n - 1))
		case Cons(h, _) if(n == 1) => Stream.cons(h(), Stream.empty)
		case _ => Stream.empty
	}
	
	def drop(n: Int): Stream[A] = this match {
		case Cons(h,t) if(n > 0) => t().drop(n - 1)
		case _ => this
	}
	
	def takeWhile(p: A => Boolean): Stream[A] = this match {
		case Cons(h,t) if(p(h())) => Stream.cons(h(), t().takeWhile(p))
		case _ => Stream.empty
	}
	
	def exists(p: A => Boolean): Boolean = this match {
		case Cons(h,t) => p(h()) || t().exists(p)
		case _ => false
	}
	
	def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h,t) => if(p(h)) Stream.cons(h,t) else Stream.empty)
	
	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}
	
	def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
	def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
		lazy val head = h
		lazy val tail = t
		Cons(() => head, () => tail)
	}
	
	def empty[A]: Stream[A] = Empty
	
	def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}