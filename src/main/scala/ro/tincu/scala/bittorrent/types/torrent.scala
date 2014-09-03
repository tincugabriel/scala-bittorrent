package ro.tincu.scala.bittorrent.types

import ro.tincu.scala.bittorrent.types.bencoded.BEncodedDict
import ro.tincu.scala.bittorrent.types.bencoded.BEncoded

/**
 * Created by gabriel on 03.09.2014.
 */
object torrent {
  //  http://www.bittorrent.org/beps/bep_0015.html
  //  Before announcing or scraping, you have to obtain a connection ID.
  //  Choose a random transaction ID.
  //  Fill the connect request structure.
  //    Send the packet.
  //    connect request:
  //
  //    Offset  Size            Name            Value
  //  0       64-bit integer  connection_id   0x41727101980
  //  8       32-bit integer  action          0 // connect
  //  12      32-bit integer  transaction_id
  //  16

  def main(args: Array[String]) {
    println(getTorrentPiecesData(Map("private"->0, "piece length" -> 100, "pieces" -> "abcdef")).getOrElse("Error parsing !"))
    println(getTorrentPiecesData(Map("private"->1, "piece length" -> 100, "pieces" -> "abcdef")).getOrElse("Error parsing !"))
    println(getTorrentPiecesData(Map("private"->1, "piece length" -> 100)).getOrElse("Missing pieces field !"))
    println(getTorrentPiecesData(Map("private"->1, "pieces" -> "ffffff")).getOrElse("Missing piece length field !"))
    println(getTorrentFileInfo(Map("path"->"/foo/bar", "length" -> 100)).getOrElse("Error parsing !"))
  }

  class Trackers(main : String, backup : List[List[String]]){}

  class Torrent(val announce : String,val announceList : List[List[String]],val info : TorrentInfo,
                val creationDate : Long,val comment : String,
                val createdBy : String,val encoding : String) {
    def this(announce : String, announceList: List[List[String]], info : TorrentInfo) =
      this(announce, announceList, info, 0, "", "", "")
    def this(announce : String, info : TorrentInfo) = this(announce, Nil, info)
  }

  class TorrentInfo(val piecesInfo : TorrentPiecesData,val name : String,
                    val files : List[TorrentFileInfo]){
    def this(piecesInfo : TorrentPiecesData, name: String,
             length : Int, md5sum : String) = this(piecesInfo, name,
      List(new TorrentFileInfo(name, length, md5sum)))
  }

  class TorrentFileInfo(val path : String,val length : Int,val md5sum : String){
    def this(path : String, length : Int) = this(path, length, "")
    override def toString =
      s"TorrentFileInfo : { path : $path, length : $length, md5sum : ${if(md5sum.isEmpty) "[missing]" else md5sum }"
  }

  class TorrentPiecesData(pieceLength : Int, pieces : String, isPrivate : Boolean){
    def this(pieceLength : Int, pieces: String) = this(pieceLength, pieces, false)
    override def toString = s"TorrentPiecesData : { piece length : $pieceLength , private : $isPrivate , pieces : $pieces }"
  }

  def getTorrentPiecesData(info : Map[String,Any]) : Option[TorrentPiecesData] = {
    val isPrivate = info.getOrElse("private",0).asInstanceOf[Int]
    val maybePieces = info.get("pieces").asInstanceOf[Option[String]]
    val maybePieceLength = info.get("piece length").asInstanceOf[Option[Int]]
    for {
      pieces <- maybePieces
      pieceLength <- maybePieceLength
    } yield new TorrentPiecesData(pieceLength, pieces, isPrivate==1)
  }

  def getTorrentFileInfo(dict : Map[String, Any]) : Option[TorrentFileInfo] = {
    val md5sum = dict.getOrElse("md5sum","").asInstanceOf[String]
    val maybeLength = dict.get("length").asInstanceOf[Option[Int]]
    val maybePath = dict.get("path").asInstanceOf[Option[String]]
    for {
      length <- maybeLength
      path <- maybePath
    } yield new TorrentFileInfo(path, length, md5sum)
  }
}
