package com.thoughtworks.dsl
package keywords
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{
  AsynchronousByteChannel,
  AsynchronousServerSocketChannel,
  AsynchronousSocketChannel,
  CompletionHandler
}

import com.thoughtworks.dsl.Dsl.{!!, Keyword}

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

  private def completionHandler[Value](successHandler: Value => (Unit !! Throwable)) = {
    new CompletionHandler[Value, Throwable => Unit] {
      def failed(exc: Throwable, failureHandler: Throwable => Unit): Unit = {
        failureHandler(exc)
      }

      def completed(result: Value, failureHandler: Throwable => Unit): Unit = {
        val protectedContinuation = try {
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

  implicit def asynchronousIoDsl[Value]: Dsl[AsynchronousIo[Value], Unit !! Throwable, Value] = {
    (keyword, handler) =>
      keyword.start(_, completionHandler(handler))
  }

}
