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
	
	def exists2(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)
	
	def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h,t) => if(p(h)) Stream.cons(h,t) else Stream.empty)
	
	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}
	
	def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
	
	def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)
	
	def map[B](p: A => B): Stream[B] = foldRight(Stream.empty[B])((h,t) => Stream.cons(p(h), t))
	
	def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h,t) => if(p(h)) Stream.cons(h,t) else t)
	
	def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h,t) => p(h) append t)
	
	def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => Stream.cons(h,t))
	
	def find(p: A => Boolean): Option[A] = filter(p).headOption
	
	def constant[A](a: A): Stream[A] =  { lazy val tail: Stream[A] = Cons(() => a, () => tail); tail }
	
	def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
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
	
	val fibs = {
		def go(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, go(f1, f0 + f1))
		go(0, 1)
	}
	
	def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
		case Some((h,s)) => cons(h, unfold(s)(f))
		case None => empty
	}
	
	def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}