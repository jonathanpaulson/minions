
class RicherSeq[T](val seq: Seq[T]) {
  def findMap[U](f: T => Option[U]): Option[U] = seq.iterator.map(f).find(_.nonEmpty).flatten
}
class RicherList[T](val list: List[T]) {
  def findMap[U](f: T => Option[U]): Option[U] = list.iterator.map(f).find(_.nonEmpty).flatten
}
class RicherArray[T](val arr: Array[T]) {
  def findMap[U](f: T => Option[U]): Option[U] = arr.iterator.map(f).find(_.nonEmpty).flatten
}

class RicherMap[T,U](val map: Map[T,U]) {
  def update(key:T)(f: Option[U] => U): Map[T,U] = map + (key -> (f(map.get(key))))
  def change(key:T)(f: Option[U] => Option[U]): Map[T,U] = {
    f(map.get(key)) match {
      case None => map - key
      case Some(v) => map + (key -> v)
    }
  }
}

object RichImplicits {
  implicit def richifyList[T](list: List[T]) = new RicherList(list)
  implicit def richifyArray[T](arr: Array[T]) = new RicherArray(arr)
  implicit def richifySeq[T](seq: Seq[T]) = new RicherSeq(seq)
  implicit def richifyMap[T,U](map: Map[T,U]) = new RicherMap(map)

  //Also just a useful function
  def assertUnreachable(): Nothing = {
    throw new Exception("Bug - reached  code that should be unreachable")
  }
}
