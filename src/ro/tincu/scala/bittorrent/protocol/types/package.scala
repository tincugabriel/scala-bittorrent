package ro.tincu.scala.bittorrent.protocol


/**
 * Created by gabriel on 8/30/14.
 */
package object types {

  def main(args: Array[String]) = {
    val list = BEncodedList(List(BEncodedInt(5000), BEncodedString("Fooooooo")));
    val map = Map[BEncodedString, BEncoded]((BEncodedString("100"), BEncodedString("5000")), (BEncodedString("bar"), list))
//    println(BEncodedDict(map))
    val (sameList, rest) = BEncoded.decodeList(list.toString).get
    println(BEncoded.decodeDict(BEncodedDict(map).toString))
//    sameList.value.foreach( x => println(x))
  }

  abstract class BEncoded {}

  case class BEncodedInt(value: Int) extends BEncoded {
    override def toString = s"i${value}e";
  }

  case class BEncodedString(value: String) extends BEncoded {
    override def toString = s"${value.length}:$value"
  }

  case class BEncodedList(value: List[BEncoded]) extends BEncoded {
    override def toString = value.foldLeft[String]("l")((x, y) => x + y.toString) + "e"
  }

  case class BEncodedDict(value: Map[BEncodedString, BEncoded]) extends BEncoded {
    override def toString = value.foldLeft[String]("d")((x, y) => x + s"${y._1.toString}${y._2.toString}") + "e"
  }

  class Torrent(announce : String, announceList : List[List[String]], info : TorrentInfo,
                creationDate : Long, comment : String,
                createdBy : String, encoding : String) {
    def this(announce : String, announceList: List[List[String]], info : TorrentInfo) =
       this(announce, announceList, info, 0, "", "", "")
    def this(announce : String, info : TorrentInfo) = this(announce, Nil, info)
  }

  class TorrentInfo(pieces : List[Byte], isPrivate : Boolean, pieceLength : Int, name : String,
                    files : List[TorrentFileInfo]){
    def this(pieces : List[Byte], isPrivate : Boolean, pieceLength : Int, name: String,
             length : Int, md5sum : String) = this(pieces, isPrivate, pieceLength, name, List(new TorrentFileInfo(name, length, md5sum)))
  }

  class TorrentFileInfo(path : String, length : Int, md5sum : String){
    def this(path : String, length : Int) = this(path, length, "")
  }

  object BEncoded {

    private val decoders = Map[Char, String => Option[(BEncoded, String)]](
      ('i', decodeInt),('l', decodeList), ('d', decodeDict))

    def decode(value: String): Option[Any] = {
      getNextDecoderMethod(value.head)(value) match {
        case Some((bdecode,_)) => Some(toValue(bdecode))
        case _ => None
      }
    }

    def toValue(bEncoded : BEncoded) : Any = bEncoded match {
      case BEncodedInt(value) => value
      case BEncodedString(value) => value
      case BEncodedList(value) => value map toValue
      case BEncodedDict(value : Map[BEncodedString, BEncoded]) => value.map((k :BEncodedString, v: BEncoded) => (k.value, toValue(v)))
    }

    private def toTorrent(value : Map[String, Any]) : Option[Torrent] = {
      None
    }

    private def getTorrentInfo(value : Map[String, Any]) : Option[TorrentInfo] = value.get("info") match {
      case None => None
      case Some(info : Map[String, Any]) => {
        val pieces = info.get("pieces")
        val pieceLength = info.get("piece length")
        val files = info.get("files")
        val md5sum = info.get("md5sum")
        val length = info.get("length")
        (pieces, pieceLength, files, md5sum, length) match {
          case (Some(data : String), Some(l : Int), Some)
        }
      }
      case _ => None
    }

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
        case _ => {
          val method = getNextDecoderMethod(rest.head)
          val result = method(rest)
          result match {
            case None => None
            case Some((value : BEncoded, str : String)) => innerDecodeList(str, acc:+value)
          }
        }
      }
      innerDecodeList(remaining.tail, List())
    }

    def decodeDict(remaining : String) : Option[(BEncodedDict, String)] = {
      def innerDecodeDict(rest: String, acc : List[(BEncodedString, BEncoded)]) : Option[(List[(BEncodedString, BEncoded)], String)] = {
        rest.head match {
          case 'e' => Some((acc, rest.tail))
          case _ => decodeString (rest) match {
            case None => None
            case Some ((key: BEncodedString, remain: String) ) => getNextDecoderMethod (remain.head) (remain) match {
              case None => None
              case Some ((value: BEncoded, remain2: String) ) => innerDecodeDict (remain2, acc :+ (key, value) )
            }
          }
        }
      }
      innerDecodeDict(remaining.tail, List()) match {
        case None => None
        case Some((list : List[(BEncoded,BEncoded)],str)) => Some((BEncodedDict(Map[BEncodedString, BEncoded](list : _*)), str))
      }
    }

    def getNextDecoderMethod(c : Char) : String=>Option[(BEncoded, String)] = decoders.getOrElse(c, decodeString)
  }

}
