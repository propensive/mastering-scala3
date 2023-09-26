package training

import language.experimental.saferExceptions

import scala.compiletime.ops.string.Matches
import scala.compiletime.* 

@main
def run(): Unit =
  try
    val file: DiskFile = path"records.tsv"

    val records = schema"schema.tsv".read(path"records.tsv")

    val record0 = records(0)
    val record1 = records(1)
    println(record0)
    println(record1)

  catch
    case error: DiskError         => println("The file could not be read from disk")
    case error: NotFoundError     => println("The file was not found")
    case error: TsvError          => println("The TSV file contained rows of different lengths")
    case error: BadIntError       => println(s"The value ${error.string} is not a valid integer")
    case error: BadRoleError      => println(s"The row contained an invalid role")
    //case error: BadFilenameError  => println(s"The filename is not valid")
    case error: UnknownFieldError => println(s"The field ${error.field} does not exist")
