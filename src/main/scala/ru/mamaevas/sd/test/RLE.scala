package ru.mamaevas.sd.test

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.Source

import scala.concurrent.{ExecutionContext, Future}

/**
 * Run-length encoding
 * На вход ф-ции дается строка произвольной длины, состящая строго из букв английскго алфавита.
 * На выходе необходимо получить "сжатую" строку, в которой одинаковые подряд идущие символы
 * заменены на пару <символ>+<количество повторений>, если повторений больше чем 1, в остальных
 * случаях символ остается в строке без изменений
 */
object RLE {

  def rle(s: String): String = s.rle

  implicit class RleStringExt(s: String) {
    def rle: String =
      s.toSeq.rle

    def rlePar(grouped: Int = 1000, parallelism: Int = 10)(implicit ec: ExecutionContext, mat: Materializer): Future[String] =
      Source(s)
        .toRlePairsAsync(grouped, parallelism)
        .map(_.map(RLE.toString).mkString)
  }

  implicit class RleLazyListExt(stream: Seq[Char]) {
    def toRlePairs: List[(Char, Int)] = {
      val res: (List[(Char, Int)], Option[(Char, Int)]) =
        stream.foldLeft((List.empty[(Char, Int)], Option.empty[(Char, Int)])) {
          case ((acc, buffer), next) =>
            buffer.fold((acc, Some(next, 1))) {
              case (lastChar, counter) =>
                if (lastChar == next) (acc, Some(next, counter + 1))
                else (acc :+ (lastChar, counter), Some(next, 1))
            }
        }
      res._2.fold(res._1)(res._1 :+ _)
    }

    def rle: String =
      toRlePairs
        .map(RLE.toString)
        .mkString
  }

  implicit class RleSourceExt(source: Source[Char, NotUsed]) {
    def toRlePairsAsync(grouped: Int = 1000, parallelism: Int = 10)
                       (implicit ec: ExecutionContext, mat: Materializer): Future[List[(Char, Int)]] =
      source
        .grouped(grouped)
        .mapAsync(parallelism) { chSeq =>
          Future {
            chSeq.toRlePairs
          }
        }.runFold(List.empty[(Char, Int)]) { (acc, next) =>
        acc.lastOption.fold(next) { accLast =>
          next.headOption.fold(acc) { nextHead =>
            if (accLast._1 == nextHead._1) {
              (acc.dropRight(1) :+ (accLast._1, accLast._2 + nextHead._2)) ++ next.tail
            } else acc ++ next
          }
        }
      }
  }

  private def toString(r: (Char, Int)) =
    s"${r._1}${if (r._2 == 1) "" else r._2}"

}
