package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.util.CommandLineParser.FromString

import scala.language.experimental.saferExceptions

object Opaques:
  opaque type DiskFile = String

  object DiskFile:
    def apply(name: String): DiskFile = name
    given FromString[DiskFile] = identity(_)

  extension (file: DiskFile)
    def readAs[DataType]()(using parser: Parser[DataType]): DataType throws NotFoundError | DiskError =
      process(parser.parse(read().mkString))

    def process[ResultType](block: FileChannel ?=> ResultType): ResultType throws NotFoundError | DiskError =
      val reader: RandomAccessFile = try RandomAccessFile(file, "r") catch case error: Exception => throw NotFoundError()
      val channel: FileChannel = reader.getChannel().nn

      try block(using channel)
      catch case error: Exception => throw DiskError()
      finally
        reader.close()
        channel.close()

export Opaques.DiskFile

object Parser:
  given string: Parser[String] = _.mkString

trait Parser[+DataType]:
  def parse(string: String): DataType

@main
def run(file: DiskFile): Unit =
  try println(file.readAs[Tsv]())
  catch
    case error: DiskError     => println("The file could not be read from disk")
    case error: NotFoundError => println("The file was not found")
    case error: TsvError      => println("The TSV file contained rows of different lengths")

inline def channel: FileChannel = summonInline[FileChannel]

def read()(using FileChannel): List[String] =
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10)).nn

  def recur(): List[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } :: recur()
    else List()

  recur()

object Tsv:
  def parse(string: String): Tsv throws TsvError =
    val rows = string.split("\n").nn.to(List).map(_.nn)
    val data = rows.map(_.split("\t").nn.to(List).map(_.nn))

    // check if all rows have the same length
    if data.map(_.length).to(Set).size != 1 then throw TsvError()

    Tsv(data)

  given (Parser[Tsv] throws TsvError) = parse(_)

case class Tsv(data: List[List[String]])

case class DiskError() extends Exception
case class NotFoundError() extends Exception
case class TsvError() extends Exception
