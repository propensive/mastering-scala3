package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.quoted.*
import scala.util.CommandLineParser.FromString

import scala.language.experimental.saferExceptions
import scala.language.experimental.erasedDefinitions
import scala.language.dynamics

object Opaques:
  opaque type DiskFile = String

  object DiskFile:
    def unsafe(name: String): DiskFile = name
    inline def apply(inline name: String): DiskFile = ${Macros.checkPath('name)}
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

inline def channel: FileChannel = summonInline[FileChannel]

def read()(using FileChannel): List[String] =
  val buffer: ByteBuffer = ByteBuffer.allocate(channel.size().toInt.min(10)).nn

  def recur(): List[String] =
    if channel.read(buffer) > 0
    then String(buffer.array(), 0, buffer.position).tap { _ => buffer.clear() } :: recur()
    else List()

  recur()

object Tsv:
  def parse[RowType <: Row](string: String): Tsv[RowType] throws TsvError | BadIntError | BadRoleError =
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

  given [RowType <: Row]: (Parser[Tsv[RowType]] throws TsvError | BadIntError | BadRoleError) =
    parse[RowType](_)

case class Row(indices: Map[String, Int], row: IArray[Any]) extends Selectable:
  def apply(field: String): Any = row(indices(field))
  def selectDynamic(field: String): Any = apply(field)

case class Tsv[RowType <: Row](headings: List[String], rows: List[IArray[Any]]):
  private val indices: Map[String, Int] = headings.zipWithIndex.to(Map)
  def apply(n: Int): RowType = Row(indices, rows(n)).asInstanceOf[RowType]

object Role:
  given (using CanThrow[BadRoleError]): Parser[Role] = try valueOf(_) catch Exception => throw BadRoleError()

enum Role:
  case Trainer, Developer, President

extension (context: StringContext)
  def path(): DiskFile throws BadFilenameError =
    if context.parts.head.matches("[.a-z]+") then DiskFile.unsafe(context.parts.head)
    else throw BadFilenameError()

object Macros:
  def checkPath(path: Expr[String])(using Quotes): Expr[DiskFile] =
    if path.valueOrAbort.matches("[.a-z]+") then '{DiskFile.unsafe($path)}
    else quotes.reflect.report.errorAndAbort("Not a valid filename")

case class DiskError() extends Exception
case class NotFoundError() extends Exception
case class UnknownFieldError(field: String) extends Exception
case class TsvError() extends Exception
case class BadIntError(string: String) extends Exception
case class BadRoleError() extends Exception
case class BadFilenameError() extends Exception

