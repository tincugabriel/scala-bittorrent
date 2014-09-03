package ro.tincu.scala.bittorrent.types

import scala.collection.immutable.TreeMap
import scala.io.{Codec, Source}

/**
 * Created by gabriel on 03.09.2014.
 */
object bencoded {
  def main(args : Array[String]) {
    val src = Source.fromFile("/home/gabriel/workspace/scala-bittorrent/src/main/resources/sample.torrent")(Codec.ISO8859)
    val dict = BEncoded.decodeDict(src.mkString)
//    println(dict.getOrElse("Fock"))
    BEncoded.decodeDict("d8:completei238e10:incompletei1e8:intervali1800e5:peers6:�L_���e").getOrElse("foo")
  }

  trait BEncoded {}

  case class BEncodedInt(val value: Int) extends BEncoded with Ordered[BEncodedInt]{
    override def toString = s"i${value}e";
    def compare(that : BEncodedInt) = value compareTo that.value
  }

  case class BEncodedString(val value: String) extends BEncoded with Ordered[BEncodedString]{
    override def toString = s"${value.length}:$value"
    def compare(that : BEncodedString) = value compareTo that.value
  }

  case class BEncodedList(val value: List[BEncoded]) extends BEncoded {
    override def toString = value.foldLeft[String]("l")((x, y) => x + y.toString) + "e"
  }

  case class BEncodedDict(val value: Map[BEncodedString, BEncoded]) extends BEncoded {
    override def toString = value.foldLeft[String]("d")((x, y) => x + s"${y._1.toString}${y._2.toString}") + "e"
  }


  object BEncoded {

    private val decoders = Map[Char, String => Option[(BEncoded, String)]](
      ('i', decodeInt),('l', decodeList), ('d', decodeDict))


    def toValue(bEncoded : BEncoded) : Any = bEncoded match {
      case BEncodedInt(value) => value
      case BEncodedString(value) => value
      case BEncodedList(value) => value map toValue
      case BEncodedDict(value : Map[BEncodedString, BEncoded]) => value.map(mapKV)
    }

    def mapKV(k : (BEncodedString,BEncoded)) : (String, Any) = (k._1.value, toValue(k._2))

    def decodeInt(remaining: String): Option[(BEncodedInt, String)] = {
      val stringForm = remaining.tail.takeWhile(c => c.isDigit || c=='-')
      if (stringForm.isEmpty) None else Some((BEncodedInt(stringForm.toInt), remaining.drop(stringForm.length + 2)))
    }

    def decodeString(remaining: String): Option[(BEncodedString, String)] = {
      val strLen = remaining.takeWhile(c => c.isDigit)
      if (strLen.isEmpty) None else Some((BEncodedString(remaining.drop(strLen.length + 1).take(strLen.toInt)), remaining.drop(strLen.length + 1 + strLen.toInt)))
    }

    def decodeList(remaining : String) : Option[(BEncodedList, String)] = {
      def innerDecodeList(rest : String, acc : List[BEncoded]) : Option[(BEncodedList, String)] = rest.head match {
        case 'e' => Some((BEncodedList(acc), rest.tail))
        case _ => getNextDecoderMethod(rest.head)(rest).
          flatMap( x=> innerDecodeList(x._2, acc:+x._1))
      }
      innerDecodeList(remaining.tail, List())
    }

    def decodeDict(remaining : String) : Option[(BEncodedDict, String)] = {
      def innerDecodeDict(rest: String, acc : List[(BEncodedString, BEncoded)]) : Option[(List[(BEncodedString, BEncoded)], String)] = {
        rest.head match {
          case 'e' => Some((acc, rest.tail))
          case  _ => {
            for {
              (key,remain)<-decodeString(rest)
              (value, otherRemain)<-getNextDecoderMethod(remain.head)(remain)
              k<-innerDecodeDict(otherRemain, acc:+(key, value))}
            yield k
          }
        }
      }
      innerDecodeDict(remaining.tail, List.empty[(BEncodedString,BEncoded)]).map( x=> (BEncodedDict(TreeMap(x._1:_*)),x._2))
    }

    def getNextDecoderMethod(c : Char) : String=>Option[(BEncoded, String)] = decoders.getOrElse(c, decodeString)
  }
}
