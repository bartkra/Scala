import scala.io.Source
import scala.util.hashing.MurmurHash3
import scala.math.max
import scala.math.pow
import scala.math.log

object HyperLogLog {
  def HyperLogLog(data :Array[String],b:Int,alpha:Double,seed:Int): Double ={ // standard hyperloglog algorithm (easiest)
    val m :Int = pow(2,b).toInt
    val M: Array[Int] = Array.fill[Int](m)(Integer.MIN_VALUE)
    for (str <- data) {
      val x = MurmurHash3.stringHash(str,seed).toBinaryString.reverse.padTo(32,'0').reverse
      val j = Integer.parseInt(x.substring(0,b),2)
      val w = x.substring(b)
      M(j)= max(M(j),rho(w))
    }
    val Z = 1.0 / M.map(x => pow(2,-x)).fold(0.0)(_+_)
    alpha*pow(m,2) * Z
  }
  def HyperLogLog2(data :Array[String],b:Int,alpha:Double,seed:Int): (Double,Double) ={ //hyperloglog with different Initialization of registers, small range corrections and large range corrections
    val m :Int = pow(2,b).toInt
    val M: Array[Int] = Array.fill[Int](m)(0)
    for (str <- data) {
      val x = MurmurHash3.stringHash(str,seed).toBinaryString.reverse.padTo(32,'0').reverse
      val j =  Integer.parseInt(x.substring(0,b),2)
      val w = x.substring(b)
      M(j)= max(M(j),rho2(w))
    }
    val Z = 1.0 / M.map(x => pow(2,-x)).fold(0.0)(_+_)
    val E = alpha*pow(m,2)* Z
    val err = 1.04/pow(m,0.5)
    if (E <= 2.5*m) {
      val V = M.count(x => x==0)
      if (V > 0)
        (m*log(m/V),err)
      else
        (E,err)
    }
    else if (E <= pow(2,32) / 30)
      (E,err)
    else
      (-pow(2,32)*log(1-E/pow(2,32)),err)
  }

  def rho(b:String) : Int ={//rho function - returns position of first 1 in string, else -infty (first HLL)
    for ((x,i) <- b.view.zipWithIndex){
      if (x == '1')
        return i + 1
    }
    Integer.MIN_VALUE
  }

  def rho2(b:String) : Int ={//rho function - returns position of first 1 in string, else position after last element of the string (second HLL)
    for ((x,i) <- b.view.zipWithIndex){
      if (x == '1')
        return i + 1
    }
    b.length + 1
  }
  def main (args: Array[String]) : Unit ={ //tests
    val source = "lbl-pkt-4.tcp"
    val bufferedSource = Source.fromFile(source)
    val lines = bufferedSource.getLines.toArray
    bufferedSource.close
    val b = 10
    val alpha = 0.709
    val data = lines.map(x=>x.split(" ").slice(1,3))

    val HLL1_source = HyperLogLog(data.map(x => x(0)),b,alpha,1758513244)
    val HLL1_dest = HyperLogLog(data.map(x => x(1)),b,alpha,509303580)
    val HLL1_pair = HyperLogLog(data.map(x => x.mkString(" ")),b,alpha,-1829788949)

    val HLL2_source = HyperLogLog2(data.map(x => x(0)),b,alpha,-889347350)
    val HLL2_dest = HyperLogLog2(data.map(x => x(1)),b,alpha,-2040983176)
    val HLL2_pair = HyperLogLog2(data.map(x => x.mkString(" ")),b,alpha,2121678573)

    println("Number of different (***): HLL1 value; HLL2 value")
    println(s"source: ${HLL1_source}; ${HLL2_source._1}")
    println(s"destination: ${HLL1_dest}; ${HLL2_dest._1}")
    println(s"pairs: ${HLL1_pair}; ${HLL2_pair._1}")
  }
}
