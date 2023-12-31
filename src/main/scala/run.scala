package training

import scala.language.experimental.saferExceptions

case class Employee(name: String, age: Int, role: Role)

@main
def run(): Unit =
  try
    val file = path"records.tsv"
    type MyRow = Row { def name: String; def age: Int; def role: Role }
    val records = file.readAs[Tsv[MyRow]]()
    val record0 = records(0).as[Employee]
    val record1 = records(1).as[Employee]
    println(record0)
    println(record1)
  catch
    case error: DiskError         => println("The file could not be read from disk")
    case error: NotFoundError     => println("The file was not found")
    case error: TsvError          => println("The TSV file contained rows of different lengths")
    case error: BadIntError       => println(s"The value ${error.string} is not a valid integer")
    case error: BadRoleError      => println(s"The row contained an invalid role")
    case error: BadFilenameError  => println(s"The filename is not valid")
    case error: UnknownFieldError => println(s"The field ${error.field} does not exist")

