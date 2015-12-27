package com.grobster.fpinscala.stream

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