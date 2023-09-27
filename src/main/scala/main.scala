package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*
import scala.compiletime.*
import scala.util.CommandLineParser.FromString

import scala.quoted.*

import scala.language.experimental.saferExceptions
import scala.language.experimental.erasedDefinitions
import scala.language.dynamics

inline def percent(inline n: Int): Unit = ${Macros.percent('n)}

object Macros:

  def percent(n: Expr[Int])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val value = n.valueOrAbort
    if value > 100 || value < 0 then quotes.reflect.report.errorAndAbort("Bad percentage")
    '{()}

  def checkFilename(context: Expr[StringContext])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val nameString = context.valueOrAbort.parts.head
    if nameString.contains(" ")
    then quotes.reflect.report.errorAndAbort(s"The filename '${nameString}' contains a space")
    
    if nameString.startsWith("http") then '{new java.net.URL(${Expr(nameString)})}
    else '{DiskFile.unsafe(${Expr(nameString)})}

  def makeSchema(context: Expr[StringContext])(using Quotes): Expr[Any] =
    import unsafeExceptions.canThrowAny
    val schemaName: String = context.valueOrAbort.parts.head
    val schema = DiskFile.unsafe(schemaName).readAs[String]()
    val types = schema.split("\t").nn.map(_.nn).to(List)

    def makeType(typeNames: List[String]): Type[?] = typeNames match
      case Nil          => Type.of[EmptyTuple]
      case head :: tail =>
        val tailType = makeType(tail)
        val headType = head match
          case "String" => Type.of[String]
          case "Int"    => Type.of[Int]
          case "Role"   => Type.of[Role]
        
        tailType match
          case '[type tupleType <: Tuple; tupleType] =>
            headType match
              case '[headType] => Type.of[headType *: tupleType]

    def make(typeNames: List[String], iarray: Expr[IArray[Any]], n: Int = 0): Expr[Tuple] = typeNames match
      case Nil          =>
        '{EmptyTuple}

      case head :: tail =>
        val tailExpr = make(tail, iarray, n + 1)
        
        val headType = head match
          case "String" => Type.of[String]
          case "Int"    => Type.of[Int]
          case "Role"   => Type.of[Role]

        headType match
          case '[headType] =>
            '{$iarray(${Expr(n)}).asInstanceOf[headType] *: $tailExpr}

    makeType(types) match
      case '[type tupleType <: Tuple; tupleType] =>
        '{ new Schema[tupleType]((iarray: IArray[Any]) => ${make(types, 'iarray, 0)}.asInstanceOf[tupleType])}

object Opaques:
  opaque type DiskFile = String

  object DiskFile:
    // inline def apply(inline name: String): DiskFile =
    //   ${Macros.checkFilename('name)}

    def unsafe(name: String): DiskFile = name

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

  given (Parser[Tsv] throws TsvError | BadIntError | BadRoleError) =
    parse(_)

case class Row(indices: Map[String, Int], row: IArray[Any]) extends Selectable:
  def apply(field: String): Any = row(indices(field))
  def selectDynamic(field: String): Any = apply(field)

case class Tsv(headings: List[String], rows: List[IArray[Any]]):
  private val indices: Map[String, Int] = headings.zipWithIndex.to(Map)

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
case class BadFilenameError() extends Exception

class Schema[+RowType <: Tuple](accessor: IArray[Any] => RowType):
  def read(diskFile: DiskFile): List[RowType] =
    import unsafeExceptions.canThrowAny
    diskFile.readAs[Tsv]().rows.map(accessor)

extension (inline context: StringContext)
  transparent inline def path(): Any = ${Macros.checkFilename('context)}
  transparent inline def schema(): Any = ${Macros.makeSchema('context)}