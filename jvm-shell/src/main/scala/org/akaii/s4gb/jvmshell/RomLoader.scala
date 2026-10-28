package org.akaii.s4gb.jvmshell

import java.nio.file.{Files, Path}
import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal

final case class RomData(name: String, bytes: ArraySeq[Byte])

enum RomError {
  case ReadFailed(path: Path, cause: Throwable)
}

object RomLoader {

  def read(path: Path): Either[RomError, RomData] =
    try Right(RomData(path.getFileName.toString, ArraySeq.unsafeWrapArray(Files.readAllBytes(path))))
    catch case NonFatal(error) => Left(RomError.ReadFailed(path, error))
}
