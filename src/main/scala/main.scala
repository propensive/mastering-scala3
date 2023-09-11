package training

import java.io.*, java.nio.*, file.*, channels.*, charset.*

import scala.util.chaining.*

@main
def run(): Unit =
  println(read())

def read(): String = ""
/*
    try (RandomAccessFile reader = new RandomAccessFile("src/test/resources/test_read.in", "r");
        FileChannel channel = reader.getChannel();
        ByteArrayOutputStream out = new ByteArrayOutputStream()) {

        int bufferSize = 1024;
        if (bufferSize > channel.size()) {
           bufferSize = (int) channel.size();
        }
        ByteBuffer buff = ByteBuffer.allocate(bufferSize);

        while (channel.read(buff) > 0) {
            out.write(buff.array(), 0, buff.position());
            buff.clear();
        }

     return new String(out.toByteArray(), StandardCharsets.UTF_8);
    }
*/
