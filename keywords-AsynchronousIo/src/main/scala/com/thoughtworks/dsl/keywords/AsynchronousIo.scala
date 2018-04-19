package com.thoughtworks.dsl
package keywords
import java.io.IOException
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{
  AsynchronousByteChannel,
  AsynchronousServerSocketChannel,
  AsynchronousSocketChannel,
  CompletionHandler
}
import Shift.implicitShift
import Dsl.{!!, Keyword}

import scala.util.control.NonFatal

trait AsynchronousIo[Value] extends Any with Keyword[AsynchronousIo[Value], Value] {

  /** Starts the asynchronous operations */
  protected def start[Attachment](attachment: Attachment, handler: CompletionHandler[Value, _ >: Attachment]): Unit
}

object AsynchronousIo {

  final case class Connect(socket: AsynchronousSocketChannel, remote: SocketAddress) extends AsynchronousIo[Void] {
    protected def start[Attachment](attachment: Attachment, handler: CompletionHandler[Void, _ >: Attachment]): Unit = {
      socket.connect(remote, attachment, handler)
    }
  }

  final case class Accept(socket: AsynchronousServerSocketChannel)
      extends AnyVal
      with AsynchronousIo[AsynchronousSocketChannel] {
    protected def start[Attachment](attachment: Attachment,
                                    handler: CompletionHandler[AsynchronousSocketChannel, _ >: Attachment]): Unit = {
      socket.accept(attachment, handler)
    }
  }

  final case class Read(channel: AsynchronousByteChannel, destination: ByteBuffer) extends AsynchronousIo[Integer] {
    protected def start[Attachment](attachment: Attachment,
                                    handler: CompletionHandler[Integer, _ >: Attachment]): Unit = {
      channel.read(destination, attachment, handler)
    }
  }

  final case class Write(channel: AsynchronousByteChannel, destination: ByteBuffer) extends AsynchronousIo[Integer] {
    protected def start[Attachment](attachment: Attachment,
                                    handler: CompletionHandler[Integer, _ >: Attachment]): Unit = {
      channel.write(destination, attachment, handler)
    }
  }

  implicit def asynchronousIoDsl[Value]: Dsl[AsynchronousIo[Value], (Unit !! Throwable), Value] =
    new Dsl[AsynchronousIo[Value], Unit !! Throwable, Value] {
      def interpret(keyword: AsynchronousIo[Value], attachment: Value => (Unit !! Throwable)): (Unit !! Throwable) = {
        failureHandler =>
          keyword.start(
            attachment,
            new CompletionHandler[Value, Value => (Unit !! Throwable)] {
              def failed(exc: Throwable, attachment: Value => (Unit !! Throwable)): Unit = {
                failureHandler(exc)
              }

              def completed(result: Value, attachment: Value => (Unit !! Throwable)): Unit = {
                val protectedContinuation = try {
                  attachment(result)
                } catch {
                  case NonFatal(e) =>
                    return failureHandler(e)
                }
                protectedContinuation(failureHandler)
              }
            }
          )
      }
    }

}
