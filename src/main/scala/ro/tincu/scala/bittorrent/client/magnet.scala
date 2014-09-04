package ro.tincu.scala.bittorrent.client

import java.net.URLDecoder

import ro.tincu.scala.bittorrent.types.bencoded.{BEncodedString, BEncoded}

/**
 * Created by gabriel on 04.09.2014.
 */
object magnet {
  def main(args : Array[String])  {
    val testLinkHTTPProtocol = "http://torrent.ubuntu.com:6969/announce?info_hash=%1B%9C%D9%D1%5Dp%1Au%1F%AD%82%91%89%7FP/%3F%E1%A9%FA&peer_id=12345678901234567890&port=7777&compact=1&downloaded=0&uploaded=0&left=0&numwant=1"
    val testLink = "magnet:?xt=urn:btih:9c2cc846e6cb91bcd9157fefadb820279ee529b0&dn=Edge+of+Tomorrow+%282014%29+720pTS-2-DVD+DD2.0+NL+Subs+NLU002&" +
      "tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80&tr=udp%3A%2F%2Ftracker.publicbt.com%3A80&tr=udp%3A%2F%2Ftracker.istole.it%3A6969&"+
      "tr=udp%3A%2F%2Fopen.demonii.com%3A1337"
  }

  object MagnetUri {
    private def fromString(queryString : String) : Option[Map[String, Array[String]]] = {
      val split = queryString.split("\\?")
      if(split.length!=2 || split(0).toLowerCase!="magnet:") None
      val pairs = split(1).split("&") map( x => (x.take(x indexOf '='), URLDecoder.decode(x.drop(x.indexOf('=') + 1),"UTF-8")))
      Some(pairs.groupBy((p => p _1)).map(x => (x._1,x._2.map(k=>k._2))))
    }

    def apply(queryString : String) = fromString(queryString).map( dict =>{
        val pattern = "urn:btih:(\\S+)".r
        val maybeName = dict.get("dn")
        val maybeUrn = dict.get("xt")
        val maybeTrackers = dict.get("tr")
        for{
          name <- maybeName
          urn <-maybeUrn
          trackers <- maybeTrackers
        } yield {
          if(Array(name.length, urn.length, trackers.length).filter(_==0).length==0) None
          urn(0) match {
            case pattern(hash) => new MagnetUri(hash, trackers, name(0))
          }
        }
      })
  }

  class MagnetUri(val hash : String,val trackers : Array[String],val displayName : String) {
    // TODO => support multiple hashing schemes perhaps ?
    override def toString = {
      val tr = trackers.mkString("{ ",", "," }")
      s"hash=$hash\ndisplayName=$displayName\ntrackers=$tr"
    }
  }
}
