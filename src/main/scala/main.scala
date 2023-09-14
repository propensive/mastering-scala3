package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*

@main
def run(filename: String): Unit =
  read(filename).foreach(println)

def read(filename: String): LazyList[String] =
  val reader: RandomAccessFile = RandomAccessFile(filename, "r")
  val channel: FileChannel = reader.getChannel()
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10))

  def recur(): LazyList[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } #:: recur()
    else LazyList()

  try recur() finally
    reader.close()
    channel.close()
