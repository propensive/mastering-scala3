package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.util.CommandLineParser.FromString

object Opaques:
  opaque type DiskFile = String

  object DiskFile:
    def apply(name: String): DiskFile = name
    given FromString[DiskFile] = identity(_)

  extension (file: DiskFile) def process[ResultType](block: FileChannel ?=> ResultType): ResultType =
    val reader: RandomAccessFile = RandomAccessFile(file, "r")
    val channel: FileChannel = reader.getChannel()

    try block(using channel) finally
      reader.close()
      channel.close()

export Opaques.DiskFile

@main
def run(file: DiskFile): Unit =
  file.process:
    read().foreach(println)

inline def channel: FileChannel = summonInline[FileChannel]

def read()(using FileChannel): List[String] =
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10))

  def recur(): List[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } :: recur()
    else List()

  recur()
