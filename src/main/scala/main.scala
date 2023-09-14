package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*

@main
def run(filename: String): Unit =
  process(filename):
    read().foreach(println)

def read()(using channel: FileChannel): List[String] =
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10))

  def recur(): List[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } :: recur()
    else List()

  recur()

def process[ResultType](filename: String)(block: FileChannel ?=> ResultType): ResultType =
  val reader: RandomAccessFile = RandomAccessFile(filename, "r")
  val channel: FileChannel = reader.getChannel()

  try block(using channel) finally
    reader.close()
    channel.close()
