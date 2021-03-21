package sanoitus

import java.net.ServerSocket

package object test {
  def findFreePort(): Int = {
    val socket = new ServerSocket(0)
    val port = socket.getLocalPort()
    socket.close()
    port
  }
}
