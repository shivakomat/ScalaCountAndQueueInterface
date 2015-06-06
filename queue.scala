package interview

/*
 Implement the following api for a Queue with the indicated complexities

 This should be an *immutable* queue
 */
trait Queue[T] {  
  def isEmpty: Boolean  
  def insert(t: T): Queue[T]  
  def head: Option[T]  
  def tail: Queue[T]
}

sealed case class SeqQueue[T](q:Seq[T]) extends Queue[T]{
  override def isEmpty: Boolean = q.isEmpty
  override def insert(t: T): Queue[T]  = if(isEmpty) SeqQueue(Seq(t)) else SeqQueue(q.union(Seq(t)))
  override def head: Option[T]  = if(!isEmpty) Some(q.head) else None
  override def tail: Queue[T] = if(isEmpty) SeqQueue(Seq()) else SeqQueue(q.tail)  
} 


object Queue {
  def empty[T]: Queue[T] = SeqQueue(Seq[T]())
}


object app{
  def main(args : Array[String]){
    val q = Queue.empty[Int]
    println(q.isEmpty)
    val q2 = q.insert(1)
    println(q2.head)
    val q3 = q2.insert(2)
    println(q3.head)
    println(q3.tail)
    val q4 = q3.insert(3)
    println(q4.isEmpty)
    println(q4.head)
    println(q4.tail)
  }
}
