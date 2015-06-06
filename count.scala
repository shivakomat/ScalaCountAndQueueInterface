package interview

import scala.annotation.switch
import scala.collection.immutable.HashMap

object CountCharacters {
	private def getNumber(i:Int):String=HashMap(1 -> "one",2 -> "two",3 -> "three",4 -> "four",5 -> "five",6 -> "six",7 -> "seven",8 -> "eight",9 -> "nine",10 -> "ten",11 -> "eleven",12 -> "twelve",13 -> "thirteen",14 -> "fourteen",15 -> "fifteen",16 -> "sixteen",17 -> "seventeen",18 -> "eighteen",19 -> "nineteen").getOrElse(i,"")
	private def getTenth(i:Int):String=HashMap(1 -> "ten",2 -> "twenty",3 -> "thirty",4 -> "forty",5 -> "fifty",6 -> "sixty",7 -> "seventy",8 -> "eighty",9 -> "ninety").getOrElse(i,"")
	
	
	private def getNumberCount(i:String):Int=HashMap("zero"-> 4,"one" -> 3,"two" -> 3,"three" -> 5,"four" -> 4,"five" -> 4,"six" -> 3,"seven" -> 5,"eight" -> 5,"nine" -> 4,"ten" -> 3,"eleven" -> 6,"twelve" -> 6,"thirteen" -> 8,"fourteen" -> 8,"fifteen" -> 7,"sixteen" -> 7,"seventeen" -> 9,"eighteen" -> 8,"nineteen" -> 8,
													 "twenty" -> 6,"thirty" ->6,"forty" -> 5,"fifty" -> 5,"sixty" -> 5,"seventy" -> 7,"eighty" -> 6,"ninety" ->6,
													 "hundread" -> 8, "thousand" -> 8,"million" ->7).getOrElse(i,0)
	
	def tens(i:Int):String={	  
		i match {
			case i if i>0 && i<20 => getNumber(i)
			case _ => getTenth(i.toString.toSeq.head.toString.toInt) + " " + getNumber(i.toString.toSeq.last.toString.toInt)
		}
	}
	def hundreads(i:Int):String={
	  if(i!=0){
		  val e1 = i.toString.toSeq.head.toString.toInt		 
		  val hundread = (e1 : @switch) match {
		    case i if e1==0 => ""
		    case _ => getNumber(e1) + " hundread "		    
		  }		  
		  hundread + tens(i.toString.toSeq.drop(1).toString.toInt)
	  }
	  else ""	  
	}
	def thousands(i:Int):String={
	  if(i!=0){
	    val e1 = i.toString.toSeq.head.toString.toInt
	    val thousand = (e1 : @switch) match {
	      case i if e1==0 => ""
	      case _ => getNumber(e1) + " thousand "
	    }
	    val e2 = i.toString.toSeq.drop(1).toString.toInt
	    val hundread = (e2 : @switch) match {
	      case i if e2>0 && e2<10 => getNumber(e2)
	      case _ => hundreads(e2)
	    }
	    thousand + hundread	    
	  }
	  else ""
	}
	def tenThousands(i:Int):String={
	  val e1 = i.toString.toSeq.take(2).toString.toInt
	  val tenThousand = (e1 : @switch) match {
	    case i if e1==0 => ""
	    case _ => tens(e1) + " thousand "	    
	  }
	  val e2 = i.toString.toSeq.drop(2).toString.toInt	  
	  val hundread = (e2 : @switch) match {
	    case i if e2==0 => ""
	    case i if e2>0 && e2<10 => getNumber(e2)
	    case _ => hundreads(e2)
	  }
	  tenThousand + hundread	  
	}
	
	def hundreadThousands(i:Int):String={
	  val e1 = i.toString.toSeq.take(3).toString.toInt
	  val hundreadThousands  = ( e1 : @switch) match {
	    case i if e1==0 => ""
	    case _  => hundreads(e1) + " thousand "
	  }
	  val e2 = i.toString.toSeq.drop(3).toString.toInt
	  val hundread = (e2 : @switch) match {
	    case i if e2>0 && e2<10 => getNumber(e2)
	    case _ => hundreads(e2)
	  }
	  hundreadThousands + hundread	
	}
	def millions(i:Int):String={
	  val e1 = i.toString.toSeq.reverse.drop(6).reverse.toString.toInt
	  val million = (e1 : @switch) match {
	    case i if e1==0 => ""
	    case i if e1>=100 => hundreads(e1) + " million "
	    case _ => tens(e1) + " million "
	  }
	  val e2 = i.toString.toSeq.reverse.take(3).reverse.toString.toInt
	  val hundread1 = (e2 : @switch) match {
	    case i if e2==0 => ""
	    case i if e2<100 => tens(e2)
	    case _ => hundreads(e2) 
	  }
	  val e3 = i.toString.toSeq.reverse.drop(3).take(3).reverse.toString.toInt
	  val hundread2 = (e3 : @switch) match {
	    case i if e3==0 => ""
	    case i if e3<100 => tens(e3) + " thousand "
	    case _ => hundreads(e3) + " thousand "
	  }
	  million + hundread2 + hundread1
	}
	def toWords(i:Int):String={
	  (i : @switch) match {
	    case 0 => "zero"
	    case i if i>0 && i<100 => tens(i)
	    case i if i>=100 && i<1000 => hundreads(i)
	    case i if i>=1000 && i<10000 => thousands(i)
	    case i if i>=10000 && i<100000 => tenThousands(i)
	    case i if i>=100000 && i<1000000 => hundreadThousands(i)
	    case i if i>=1000000 && i<1000000000 => millions(i)
	    case _ => throw new IllegalArgumentException(i+" -> un supported value")
	  }	  
	}
	def countCharsInWords(i: Int): Int = toWords(i).filter(_ != ' ').length
	def countCharsInWordsOptimized(i:Int):Int = toWords(i).split(' ').toSeq.map(getNumberCount(_)).sum
	
}

object count {
  def main(args: Array[String]) {	
	println(CountCharacters.toWords(0))
	println(CountCharacters.toWords(02))
	println(CountCharacters.toWords(3))
	println(CountCharacters.toWords(13))
		
	
	println(CountCharacters.toWords(100))
	println(CountCharacters.toWords(102))
	println(CountCharacters.toWords(110))
	println(CountCharacters.toWords(125))
	println(CountCharacters.toWords(220))
	println(CountCharacters.toWords(999))
	
	
	
	println(CountCharacters.toWords(1000))
	println(CountCharacters.toWords(1009))
	println(CountCharacters.toWords(1102))
	println(CountCharacters.toWords(2110))	
	println(CountCharacters.toWords(3125))
	println(CountCharacters.toWords(4220))
	
	
	println(CountCharacters.toWords(10000))
	println(CountCharacters.toWords(10002))
	println(CountCharacters.toWords(11102))
	println(CountCharacters.toWords(22110))	
	println(CountCharacters.toWords(33125))
	println(CountCharacters.toWords(54220))
	println(CountCharacters.toWords(99999))
	
	
	println(CountCharacters.toWords(100000))
	println(CountCharacters.toWords(100002))
	println(CountCharacters.toWords(411102))
	println(CountCharacters.toWords(122113))
	println(CountCharacters.toWords(333125))
	println(CountCharacters.toWords(454220))	
	println(CountCharacters.toWords(999999))
	
	
	println(CountCharacters.toWords(1000000))
	println(CountCharacters.toWords(1000002))
	println(CountCharacters.toWords(1000102))
	println(CountCharacters.toWords(1999000))
	println(CountCharacters.toWords(1000012))
	println(CountCharacters.toWords(1999999))
	println(CountCharacters.toWords(10000000))
	println(CountCharacters.toWords(23000000))
	println(CountCharacters.toWords(23999999))
	
	println(CountCharacters.toWords(100000000))
	println(CountCharacters.toWords(123000002))
	println(CountCharacters.toWords(123000022))
	
	println(CountCharacters.toWords(123000016))
	println(CountCharacters.countCharsInWords(123000016))
    println(CountCharacters.countCharsInWordsOptimized(123000016))
    
	println(CountCharacters.toWords(999999999))	
    println(CountCharacters.countCharsInWords(999999999))
    println(CountCharacters.countCharsInWordsOptimized(999999999)) 
   
  }
}
