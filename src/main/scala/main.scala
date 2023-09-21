package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

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

