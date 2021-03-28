import scala.util.hashing.MurmurHash3
import scala.io.Source


class Bloom(m:Int,k : Int, seeds_src: String) {
  val size : Int = m
  val num_hash: Int = k
  val table : Array[Boolean]=Array.fill[Boolean](m)(elem=false)
  val seeds = Source.fromFile("seeds.txt", "UTF-8").getLines.map(_.toInt).toArray
  
  def this(m:Int,k: Int,seeds_src: String,str:Array[String])={ //filter auxilary constructor with data
    this(m,k,seeds_src)
    for (obj <-str){
      for(i<-0 until this.num_hash){
        var h = MurmurHash3.stringHash(obj,seeds(i)) % this.size
        if (h<0){
          h+=this.size
        }
        table(h)=true
      }
    }
  }
  def add(obj:String):Unit= { // add element to filter
    for (i <- 0 until num_hash) {
      var h = MurmurHash3.stringHash(obj, seeds(i)) % size
      if (h < 0) {
        h += size
      }
      table(h) = true
    }
  }
  def in (obj:String):Boolean = { // checks if element is in filter
    var ans:Boolean=true
    for (i <- 0 until num_hash) {
      var h = MurmurHash3.stringHash(obj, seeds(i)) % size
      if (h < 0) {
        h += size
      }
      ans = ans && table(h)
    }
    ans
  }
}
object test {
  def main(args: Array[String]): Unit={
    val students = Source.fromFile("students.txt").getLines.toArray
    var filter = new Bloom(416,11,students)//416=16 times number of students->p[FP]=0.0004; optimal k
    //println(filter.table.mkString(" "))
    println(filter.in("Adam Małysz"))
	println(filter.in("Michael"))
  }
}