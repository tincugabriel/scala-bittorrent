package ro.tincu.scala.bittorrent.client.udp


import scala.util.Random

/**
 * Created by gabriel on 04.09.2014.
 */
object tracker {
  def main(args : Array[String]) : Unit = {
    val track = new UDPTracker("tracker.openbittorrent.com", 80, 9999)
    track.connect
  }
  import java.net.{DatagramPacket, InetSocketAddress, DatagramSocket}
  class UDPTracker(val url : String, val port : Int,val localPort : Int, val connectionId : Option[Long], val socket : DatagramSocket){
    import java.nio.ByteBuffer
    val initialConnectionId : Long = 0x0000041727101980L
    val random = new Random()
    def this(url: String, port: Int, localPort: Int, socket : DatagramSocket) = this(url, port, localPort, None, socket)
    def this(url: String, port: Int, localPort: Int) = this(url, port, localPort, None, new DatagramSocket(port))

    def connect = connectionId match {
      case Some(_) => this
      case None => {
        val transactionId = random.nextInt
        val action : Int = 0 //connect
        val buf = ByteBuffer.allocate(16)
        val arr : Array[Byte] = buf.putLong(initialConnectionId).putInt(action).putInt(transactionId).array
        arr.toList.foreach(x => println(x.asInstanceOf[Int]))
        if(!socket.isConnected) socket.connect(new InetSocketAddress(url, port))
        val packet = new DatagramPacket(arr, 16)
        socket.send(packet)
        socket.receive(packet)
        val data = packet.getData
        data.toList.foreach(x => println(x.asInstanceOf[Int]))
      }
    }
  }
}
