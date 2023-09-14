package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.util.CommandLineParser.FromString

object DiskFile:
  given FromString[DiskFile] = DiskFile(_)

case class DiskFile(filename: String)

@main
def run(filename: String): Unit =
  val file = DiskFile(filename)
  process(file):
    read().foreach(println)

inline def channel: FileChannel = summonInline[FileChannel]

def read()(using FileChannel): List[String] =
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10))

  def recur(): List[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } :: recur()
    else List()

  recur()

def process[ResultType](file: DiskFile)(block: FileChannel ?=> ResultType): ResultType =
  val reader: RandomAccessFile = RandomAccessFile(file.filename, "r")
  val channel: FileChannel = reader.getChannel()

  try block(using channel) finally
    reader.close()
    channel.close()
