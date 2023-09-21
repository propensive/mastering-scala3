package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*, java.net.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.quoted.*
import scala.deriving.*
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
    def readAs[DataType <: AnyKind]()(using resolver: Resolver[DataType])(using Parser[resolver.DataType]): resolver.DataType throws NotFoundError | DiskError =
      process(read().mkString.parseAs[resolver.DataType])

    def process[ResultType](block: FileChannel ?=> ResultType): ResultType throws NotFoundError | DiskError =
      val reader: RandomAccessFile = try RandomAccessFile(file, "r") catch case error: Exception => throw NotFoundError()
      val channel: FileChannel = reader.getChannel().nn

      try block(using channel)
      catch case error: Exception => throw DiskError()
      finally
        reader.close()
        channel.close()

export Opaques.DiskFile

object Resolver:
  given Resolver[List] with
    type DataType = List[String]
  
  given Resolver[String] with
    type DataType = String

trait Resolver[Type <: AnyKind]:
  type DataType

object Parser:
  given string: Parser[String] = _.mkString
  given listString: Parser[List[String]] = List(_)
  
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
  def parse[RowType](string: String): Tsv[RowType] throws TsvError =
    val rows = string.split("\n").nn.to(List).map(_.nn)
    val data = rows.map(_.split("\t").nn.to(List).map(_.nn))

    // check if all rows have the same length
    if data.map(_.length).to(Set).size != 1 then throw TsvError()

    Tsv(data.head, data.tail.map(IArray.from(_)))

  given [RowType]: (Parser[Tsv[RowType]] throws TsvError) = parse[RowType](_)

case class Row(indices: Map[String, Int], row: IArray[String]):
  def apply(field: String): String = row(indices(field))

  transparent inline def fields[ElementTypes <: Tuple, LabelTypes <: Tuple]: Tuple =
    inline erasedValue[ElementTypes] match
      case _: (headType *: tailType) => inline erasedValue[LabelTypes] match
        case _: (headLabel *: tailLabel) => inline valueOf[headLabel].asMatchable match
          case label: String =>
            summonInline[Parser[headType]].parse(row(indices(label))) *: fields[tailType, tailLabel]

      case _ => EmptyTuple

  transparent inline def as[RowType](using mirror: Mirror.ProductOf[RowType]): mirror.MirroredMonoType =
    mirror.fromProduct(fields[mirror.MirroredElemTypes, mirror.MirroredElemLabels])

case class Tsv[RowType](headings: List[String], rows: List[IArray[String]]):
  private val indices: Map[String, Int] = headings.zipWithIndex.to(Map)
  def apply(n: Int): RowType = Row(indices, rows(n)).asInstanceOf[RowType]

object Role:
  given (using CanThrow[BadRoleError]): Parser[Role] = try valueOf(_) catch Exception => throw BadRoleError()

enum Role:
  case Trainer, Developer, President

extension (inline context: StringContext)
  transparent inline def path(): DiskFile | URL = ${Macros.checkPath('context)}

object Macros:
  def checkPath(context: Expr[StringContext])(using Quotes): Expr[DiskFile | URL] =
    import quotes.reflect.*
    val path = context.valueOrAbort.parts.head
    
    if path.startsWith("http") then
      try URL(path) catch Exception => quotes.reflect.report.errorAndAbort("Not a valid URL")
      '{URL(${Expr(path)})}
    else if path.matches("[.a-z]+") then '{DiskFile(${Expr(path)})}
    else quotes.reflect.report.errorAndAbort("Not a valid filename")
    

case class DiskError() extends Exception
case class NotFoundError() extends Exception
case class UnknownFieldError(field: String) extends Exception
case class TsvError() extends Exception
case class BadIntError(string: String) extends Exception
case class BadRoleError() extends Exception
case class BadFilenameError() extends Exception

