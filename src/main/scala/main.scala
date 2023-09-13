package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.util.CommandLineParser.FromString

import scala.language.experimental.saferExceptions
import scala.language.experimental.erasedDefinitions
import scala.language.dynamics

object Opaques:
  opaque type DiskFile = String

  object DiskFile:
    def apply(name: String): DiskFile = name
    given FromString[DiskFile] = identity(_)

  extension (file: DiskFile)
    def readAs[DataType: Parser](): DataType throws NotFoundError | DiskError =
      process(read().mkString.parseAs[DataType])

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
  
  given int: (Parser[Int] throws BadIntError) = string =>
    try string.toInt catch Exception => throw BadIntError(string)

trait Parser[+DataType]:
  def parse(string: String): DataType

extension (string: String)
  def parseAs[ResultType](using parser: Parser[ResultType]): ResultType = parser.parse(string)

@main
def run(file: DiskFile): Unit =
  try
    val records = file.readAs[Tsv]()
    type MyRow = Row { def name: String; def age: Int; def role: Role }
    val record0 = records(0).asInstanceOf[MyRow]
    val record1 = records(1).asInstanceOf[MyRow]
    println(record0)
    println(record1)
    println(record0.role)
  catch
    case error: DiskError         => println("The file could not be read from disk")
    case error: NotFoundError     => println("The file was not found")
    case error: TsvError          => println("The TSV file contained rows of different lengths")
    case error: BadIntError       => println(s"The value ${error.string} is not a valid integer")
    case error: BadRoleError      => println(s"The row contained an invalid role")
    case error: UnknownFieldError => println(s"The field ${error.field} does not exist")

inline def channel: FileChannel = summonInline[FileChannel]

def read()(using FileChannel): List[String] =
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10)).nn

  def recur(): List[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } :: recur()
    else List()

  recur()

object Tsv:
  def parse(string: String): Tsv throws TsvError | BadIntError | BadRoleError =
    val rows = string.split("\n").nn.to(List).map(_.nn)
    val data = rows.map(_.split("\t").nn.to(List).map(_.nn))

    // check if all rows have the same length
    if data.map(_.length).to(Set).size != 1 then throw TsvError()

    val data2 = data.tail.map: cells =>
      data.head.zip(cells).map: (field, value) =>
        field match
          case "role" => value.parseAs[Role]
          case "name" => value.parseAs[String]
          case "age"  => value.parseAs[Int]

    Tsv(data.head, data2.map(IArray.from(_)))

  given (Parser[Tsv] throws TsvError | BadIntError | BadRoleError) = parse(_)

case class Row(indices: Map[String, Int], row: IArray[Any]) extends Selectable:
  def apply(field: String): Any = row(indices(field))
  def selectDynamic(field: String): Any = apply(field)

case class Tsv(headings: List[String], rows: List[IArray[Any]]):
  private val indices: Map[String, Int] = headings.zipWithIndex.to(Map)
  def apply(n: Int): Row = Row(indices, rows(n))

object Role:
  given (Parser[Role] throws BadRoleError) = try valueOf(_) catch Exception => throw BadRoleError()

enum Role:
  case Trainer, Developer, President

case class DiskError() extends Exception
case class NotFoundError() extends Exception
case class UnknownFieldError(field: String) extends Exception
case class TsvError() extends Exception
case class BadIntError(string: String) extends Exception
case class BadRoleError() extends Exception

