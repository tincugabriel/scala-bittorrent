package ro.tincu.scala.bittorrent

import java.net.URLDecoder

import scalaj.http.Http

/**
 * Created by gabriel on 9/2/14.
 */
package object client {
  def main(args : Array[String])  {
    val testLinkHTTPProtocol = "http://torrent.ubuntu.com:6969/announce?info_hash=%1B%9C%D9%D1%5Dp%1Au%1F%AD%82%91%89%7FP/%3F%E1%A9%FA" +
      "&peer_id=12345678901234567890&port=7777&compact=1&downloaded=0&uploaded=0&left=0&"
    val testLink = "magnet:?xt=urn:btih:9c2cc846e6cb91bcd9157fefadb820279ee529b0&dn=Edge+of+Tomorrow+%282014%29+720pTS-2-DVD+DD2.0+NL+Subs+NLU002&" +
      "tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80&tr=udp%3A%2F%2Ftracker.publicbt.com%3A80&tr=udp%3A%2F%2Ftracker.istole.it%3A6969&"+
      "tr=udp%3A%2F%2Fopen.demonii.com%3A1337"
    val req = Http(testLinkHTTPProtocol).asString

    MagnetUri.fromString(testLink) match {
      case None => "Error parsing"
      case Some(a) =>  a foreach(x => {x._2.foreach(println);println()})
    }
    MagnetUri.parse(testLink) match {
      case None => "Parse error"
      case Some(x) => println(x)
    }
    println("--------------------")
    println(req.take(400))
  }



  object MagnetUri {
    def fromString(queryString : String) : Option[Map[String, Array[String]]] = {
      val split = queryString.split("\\?")
      if(split.length!=2 || split(0).toLowerCase!="magnet:") None
      val pairs = split(1).split("&") map( x => (x.take(x indexOf '='), URLDecoder.decode(x.drop(x.indexOf('=') + 1),"UTF-8")))
      Some(pairs.groupBy((p => p _1)).map(x => (x._1,x._2.map(k=>k._2))))
    }

    def parse(queryString : String) = fromString(queryString) match {
      case None => None
      case Some(dict) => {
        val pattern = "urn:btih:(\\S+)".r
        val name = dict.get("dn")
        val urn = dict.get("xt")
        val trackers = dict.get("tr")
        (name, urn, trackers) match {
          case (Some(dnArr), Some(hashArr), Some(trArr)) => {
            if(dnArr.length==0 || hashArr.length ==0 || trArr.length ==0) None
            hashArr(0) match {
              case pattern(hash) => Some(new MagnetUri(hash, trArr, dnArr(0)))
              case _ => None
            }
          }
          case _ => None
        }
      }
    }
  }

  class MagnetUri(val hash : String,val trackers : Array[String],val displayName : String) {
    // TODO => support multiple hashing schemes perhaps ?
    override def toString = {
      val tr = trackers.mkString("{ ",", "," }")
      s"hash=$hash\ndisplayName=$displayName\ntrackers=$tr"
    }
  }
}
