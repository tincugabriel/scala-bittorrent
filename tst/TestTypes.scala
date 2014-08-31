import scala.io.{Codec, Source}
import ro.tincu.scala.bittorrent.protocol.types
/**
 * Created by gabriel on 8/30/14.
 */
object TestTypes {
  def main(args : Array[String]) {
    val src = Source.fromFile("/home/gabriel/workspace/untitledscala-bittorrent/resources/sample.torrent")(Codec.ISO8859)
    val dict = types.BEncoded.decode(src.mkString)
    print(dict)
  }
}