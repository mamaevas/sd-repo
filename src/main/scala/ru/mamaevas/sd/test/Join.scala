package ru.mamaevas.sd.test

/**
 * Объединение отрезков
 * На вход ф-ции подается набор отрезков числовой оси вида (a: Int, b: Int) в произвольном порядке.
 * На выходе необходимо получить набор отрезков, в которых пересекающиеся отрезки объединены.
 */
object Join {

  def join(segments: Set[(Int, Int)]): Set[(Int, Int)] =
    join(segments.toList).toSet

  def join(segments: List[(Int, Int)]): List[(Int, Int)] =
    segments
      .sorted
      .foldLeft(List.empty[(Int, Int)]) { (acc, next) =>
        acc match {
          case Nil => List(next)
          case head :: tail if head._1 <= next._1 && next._2 <= head._2 => head :: tail
          case head :: tail if next._1 <= head._2 => (head._1, next._2) :: tail
          case _ => next :: acc
        }
      }
      .reverse
}
