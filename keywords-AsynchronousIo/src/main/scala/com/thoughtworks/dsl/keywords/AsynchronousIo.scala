package com.thoughtworks.dsl
package keywords
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels._

import com.thoughtworks.dsl.Dsl.IsKeyword

import scala.util.control.NonFatal
import scala.annotation.unchecked.uncheckedVariance

/** The base keyword to perform asynchronous IO in [[domains.task.Task]]s.
  *
  * @example
  *   The following `readAll` is a [[com.thoughtworks.dsl.domains.Task Task]] to
  *   read file content with the help of [[AsynchronousIo.ReadFile]]
  *
  * {{{
  *           import java.nio._, file._, channels._
  *           import com.thoughtworks.dsl.domains.Task
  *           import com.thoughtworks.dsl.keywords._
  *           import com.thoughtworks.dsl.keywords.Shift._
  *           import com.thoughtworks.dsl.keywords.AsynchronousIo.ReadFile
  *           import scala.collection.mutable.ArrayBuffer
  *           import scala.io.Codec
  *           def readAll(channel: AsynchronousFileChannel, temporaryBufferSize: Int = 4096): Task[ArrayBuffer[CharBuffer]] = Task {
  *             val charBuffers = ArrayBuffer.empty[CharBuffer]
  *             val decoder = Codec.UTF8.decoder
  *             val byteBuffer = ByteBuffer.allocate(temporaryBufferSize)
  *             var position: Long = 0L
  *             while (!ReadFile(channel, byteBuffer, position) != -1) {
  *               position += byteBuffer.position()
  *               byteBuffer.flip()
  *               charBuffers += decoder.decode(byteBuffer)
  *               byteBuffer.clear()
  *             }
  *             charBuffers
  *           }
  * }}}
  *
  * `Task`s created from !-notation can be used in `for`-comprehension, and
  * other keywords can be used together in the same `for` block.
  *
  * For example, the following `cat` function contains a single `for` block to
  * concatenate file contents. It asynchronously iterates elements `Seq`,
  * `ArrayBuffer` and `String` with the help of [[keywords.Each]], managed
  * native resources with the help of [[keywords.Using]], performs previously
  * created `readAll` task with the help of [[keywords.Shift]], and finally
  * converts the return type [[comprehension.ComprehensionOps.as as]] a
  * `Task[Vector[Char]]`.
  *
  * {{{
  *           import com.thoughtworks.dsl._
  *           import com.thoughtworks.dsl.keywords._
  *           import com.thoughtworks.dsl.domains.Task
  *           import java.net.URL
  *           def cat(paths: Path*) = Each.ToView {
  *             for {
  *               path <- Each(paths)
  *               channel <- Using(AsynchronousFileChannel.open(path))
  *               charBuffers <- Shift(readAll(channel))
  *               charBuffer <- Each(charBuffers)
  *               char <- Each(charBuffer.toString)
  *             } yield char
  *           }.to[Task]
  * }}}
  *
  * Then the `cat` function is used to concatenate files from this project, as
  * shown below:
  *
  * {{{
  *           Task.toFuture(Task {
  *             val filesToRead = for (fileName <- Seq(".sbtopts", ".scalafmt.conf")) yield {
  *               Paths.get(sourcecode.File()).getParent.resolve("../../../../../..").resolve(fileName)
  *             }
  *             (!Shift(cat(filesToRead: _*))).mkString should be(
  *               filesToRead.map { fileToRead =>
  *                 new String(Files.readAllBytes(fileToRead), io.Codec.UTF8.charSet)
  *               }.mkString
  *             )
  *           })
  * }}}
  */
trait AsynchronousIo[+Value] extends Any with Dsl.Keyword.Trait {

  /** Starts the asynchronous operations */
  protected def start[Attachment](
      attachment: Attachment,
      // CompletionHandler is essentially covariant even though Java does not
      // support covariance
      handler: CompletionHandler[Value @uncheckedVariance, _ >: Attachment]
  ): Unit
}

object AsynchronousIo {
  given [Keyword <: AsynchronousIo[Value], Value]: IsKeyword[Keyword, Value]
  with {}
  final case class Connect(
      socket: AsynchronousSocketChannel,
      remote: SocketAddress
  ) extends AsynchronousIo[Void] {
    protected def start[Attachment](
        attachment: Attachment,
        handler: CompletionHandler[Void, _ >: Attachment]
    ): Unit = {
      socket.connect(remote, attachment, handler)
    }
  }

  final case class Accept(socket: AsynchronousServerSocketChannel)
      extends AnyVal
      with AsynchronousIo[AsynchronousSocketChannel] {
    protected def start[Attachment](
        attachment: Attachment,
        handler: CompletionHandler[AsynchronousSocketChannel, _ >: Attachment]
    ): Unit = {
      socket.accept(attachment, handler)
    }
  }

  final case class ReadFile(
      channel: AsynchronousFileChannel,
      destination: ByteBuffer,
      position: Long
  ) extends AsynchronousIo[Integer] {
    protected def start[Attachment](
        attachment: Attachment,
        handler: CompletionHandler[Integer, _ >: Attachment]
    ): Unit = {
      channel.read(destination, position, attachment, handler)
    }
  }

  final case class WriteFile(
      channel: AsynchronousFileChannel,
      source: ByteBuffer,
      position: Long
  ) extends AsynchronousIo[Integer] {
    protected def start[Attachment](
        attachment: Attachment,
        handler: CompletionHandler[Integer, _ >: Attachment]
    ): Unit = {
      channel.write(source, position, attachment, handler)
    }
  }

  final case class Read(
      channel: AsynchronousByteChannel,
      destination: ByteBuffer
  ) extends AsynchronousIo[Integer] {
    protected def start[Attachment](
        attachment: Attachment,
        handler: CompletionHandler[Integer, _ >: Attachment]
    ): Unit = {
      channel.read(destination, attachment, handler)
    }
  }

  final case class Write(channel: AsynchronousByteChannel, source: ByteBuffer)
      extends AsynchronousIo[Integer] {
    private[dsl] def destination = source

    protected def start[Attachment](
        attachment: Attachment,
        handler: CompletionHandler[Integer, _ >: Attachment]
    ): Unit = {
      channel.write(source, attachment, handler)
    }
  }

  private def completionHandler[Value](
      successHandler: Value => (Unit !! Throwable)
  ) = {
    new CompletionHandler[Value, Throwable => Unit] {
      def failed(exc: Throwable, failureHandler: Throwable => Unit): Unit = {
        failureHandler(exc)
      }

      def completed(result: Value, failureHandler: Throwable => Unit): Unit = {
        val protectedContinuation =
          try {
            successHandler(result)
          } catch {
            case NonFatal(e) =>
              val () = failureHandler(e)
              return
          }
        protectedContinuation(failureHandler)
      }
    }
  }

  given [Value]: Dsl.Original[AsynchronousIo[Value], Unit !! Throwable, Value] =
    Dsl.Original { (keyword, handler) =>
      keyword.start(_, completionHandler(handler))
    }

}
