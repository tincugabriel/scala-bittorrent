package ro.tincu.scala.bittorrent.types

import ro.tincu.scala.bittorrent.types.bencoded.BEncodedDict
import ro.tincu.scala.bittorrent.types.bencoded.BEncoded

/**
 * Created by gabriel on 03.09.2014.
 */
object torrent {
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

  private def getTorrentInfo(info : Map[String, Any]) : Option[TorrentInfo] = {
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
          ll.map(BEncoded.toValue) match {
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
}
