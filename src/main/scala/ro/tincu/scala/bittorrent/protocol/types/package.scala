package ro.tincu.scala.bittorrent.protocol.types

import scala.collection.immutable.TreeMap
import scala.io.{Codec, Source}


/**
 * Created by gabriel on 8/30/14.
 */
package object types {

  def main(args : Array[String]) {
    val src = Source.fromFile("/home/gabriel/workspace/scala-bittorrent/src/main/resources/sample.torrent")(Codec.ISO8859)
    val dict = BEncoded.decode(src.mkString)
    dict match {
      case Some(torr : Torrent) => {
        println(torr.announce)
      }
    }
  }

  abstract class BEncoded {}

  case class BEncodedInt(val value: Int) extends BEncoded {
    override def toString = s"i${value}e";
  }

  case class BEncodedString(val value: String) extends BEncoded with Ordered[BEncodedString]{
    override def toString = s"${value.length}:$value"
    def compare(that : BEncodedString) = value.compareTo(that value)
  }

  case class BEncodedList(val value: List[BEncoded]) extends BEncoded {
    override def toString = value.foldLeft[String]("l")((x, y) => x + y.toString) + "e"
  }

  case class BEncodedDict(val value: Map[BEncodedString, BEncoded]) extends BEncoded {
    override def toString = value.foldLeft[String]("d")((x, y) => x + s"${y._1.toString}${y._2.toString}") + "e"
  }

  class Torrent(val announce : String,val announceList : List[List[String]],val info : TorrentInfo,
                val creationDate : Long,val comment : String,
                val createdBy : String,val encoding : String) {
    def this(announce : String, announceList: List[List[String]], info : TorrentInfo) =
      this(announce, announceList, info, 0, "", "", "")
    def this(announce : String, info : TorrentInfo) = this(announce, Nil, info)
  }

  class TorrentInfo(val pieces : Array[Byte],val isPrivate : Boolean,val pieceLength : Int,val name : String,
                    val files : List[TorrentFileInfo]){
    def this(pieces : Array[Byte], isPrivate : Boolean, pieceLength : Int, name: String,
             length : Int, md5sum : String) = this(pieces, isPrivate, pieceLength, name, List(new TorrentFileInfo(name, length, md5sum)))
  }

  class TorrentFileInfo(path : String, length : Int, md5sum : String){
    def this(path : String, length : Int) = this(path, length, "")
  }


  object BEncoded {

    private val decoders = Map[Char, String => Option[(BEncoded, String)]](
      ('i', decodeInt),('l', decodeList), ('d', decodeDict))

    def decode(value: String): Option[Torrent] = {
      getNextDecoderMethod(value.head)(value) match {
        case Some((bdecode,_)) => {
          toValue(bdecode) match {
            case m : Map[String, Any] => toTorrent(m)
            case _ => None
          }
        }
        case _ => None
      }
    }

    def toValue(bEncoded : BEncoded) : Any = bEncoded match {
      case BEncodedInt(value) => value
      case BEncodedString(value) => value
      case BEncodedList(value) => value map toValue
      case BEncodedDict(value : Map[BEncodedString, BEncoded]) => value.map(mapKV)
    }

    def mapKV(k : (BEncodedString,BEncoded)) : (String, Any) = (k._1.value, toValue(k._2))

    def toTorrent(value : Map[String, Any]) : Option[Torrent] = {
      val announce = value.get("announce")
      val announceList  = value.getOrElse("announce-list", Nil)
      // TODO -> getTorrentInfo expects the big map
      val info = value.get("info")
      val comment = value.getOrElse("comment","")
      val creationDate = value.getOrElse("creation date",0)
      val createdBy = value.getOrElse("created by","")
      val encoding = value.getOrElse("encoding","")
      (announce, announceList, info, comment, creationDate, createdBy, encoding) match {
        case (Some(a : String),al : List[List[String]], Some(i : Map[String,Any]),
        c: String, cd : Int, cb : String, enc : String) => {
          getTorrentInfo(value) match {
            case None => None
            case Some(inf) => Some(new Torrent(a, al, inf, cd, c, cb, enc))
          }
        }
        case _ => None
      }
    }

    def getTorrentFileInfo(dict : Map[String, Any]) : Option[TorrentFileInfo] = {
      val md5sum = dict.get("md5sum")
      val length = dict.get("length")
      val path = dict.get("path")
      (md5sum, length, path) match {
        case (Some(s : String), Some(l : Int), Some(p : String)) => Some(new TorrentFileInfo(p, l, s))
        case (None, Some(l : Int), Some(p : String)) => Some(new TorrentFileInfo(p, l))
        case _ => None
      }
    }

    private def getTorrentInfo(value : Map[String, Any]) : Option[TorrentInfo] = value.get("info") match {
      case Some(info : Map[String, Any]) => {
        val pieces = info.get("pieces")
        val pieceLength = info.get("piece length")
        val files = info.get("files")
        val md5sum = info.get("md5sum")
        val length = info.get("length")
        val name = info.get("name")
        val isPrivate = info.getOrElse("private","0") == "0"
        (pieces, pieceLength, files, md5sum, length, name, isPrivate) match {
          case (Some(data : String), Some(l : Int),
          Some(ll : List[BEncodedDict]), _, _, Some(n : String), _) =>
            ll.map(toValue) match {
              case a:List[Map[String, Any]] => Some(new TorrentInfo(data.getBytes, isPrivate, l, n, a.map(getTorrentFileInfo).flatten))
              case _ => None
            }
          case (Some(data : String), Some(pill : Int),
          _ , Some(sm : String), Some(l : Int), Some(nm : String), _) =>
            Some(new TorrentInfo(data.getBytes, isPrivate, pill, nm, l, sm))
          case (Some(data : String), Some(pill : Int),
          _ , None, Some(l : Int), Some(nm : String), _) =>
            Some(new TorrentInfo(data.getBytes, isPrivate, pill, nm, l, ""))
          case _ => None
        }
      }
      case None => None
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
        case Some((list : List[(BEncoded,BEncoded)],str)) => Some((BEncodedDict(TreeMap[BEncodedString, BEncoded](list : _*)), str))
      }
    }

    def getNextDecoderMethod(c : Char) : String=>Option[(BEncoded, String)] = decoders.getOrElse(c, decodeString)
  }

}
