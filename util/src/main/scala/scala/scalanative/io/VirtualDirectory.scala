package scala.scalanative
package io

import scala.collection.mutable
import scala.collection.JavaConverters._
import java.net.URI
import java.nio.ByteBuffer
import java.nio.file._
import java.nio.channels._
import java.nio.charset.StandardCharsets
import scalanative.util.{acquire, defer, Scope}

sealed trait VirtualDirectory {
  type IRPath

  /** Check if file with given path is in the directory. */
  def contains(path: IRPath): Boolean =
    irFilePaths.contains(path)

  /** Obtain the id associated with the path of an IR file. */
  def id(path: IRPath): String

  /** Reads a contents of file with given path. */
  def read(path: IRPath)(implicit scope: Scope): ByteBuffer

  /** Replaces contents of file with given value. */
  def write(path: Path, buffer: ByteBuffer): Unit

  /** List all ir files in this directory. */
  def irFilePaths: Seq[IRPath]
}

object VirtualDirectory {

  /** Real, non-virtual directory on local file system. */
  def local(file: Path): VirtualDirectory = {
    def absolute = file.toAbsolutePath
    assert(Files.exists(file), s"Local directory doesn't exist: $absolute")
    assert(Files.isDirectory(file), s"Not a directory: $absolute")

    new LocalDirectory(file)
  }

  /** Virtual directory that represents contents of the jar file. */
  def jar(file: Path)(implicit scope: Scope): VirtualDirectory = {
    val absolute = file.toAbsolutePath
    assert(Files.exists(file), s"Jar doesn't exist: $absolute")
    assert(absolute.toString.endsWith(".jar"), s"Not a jar: $absolute")

    new JarDirectory(file)
  }

  /** Virtual directory based on either local directory or a jar. */
  def real(file: Path)(implicit in: Scope): VirtualDirectory =
    if (Files.isDirectory(file)) {
      local(file)
    } else if (file.toString.endsWith(".jar")) {
      jar(file)
    } else {
      throw new UnsupportedOperationException(
        "Neither a jar, nor a directory: " + file
      )
    }

  /** Empty directory that contains no files. */
  val empty: VirtualDirectory = EmptyDirectory

  private final class LocalDirectory(path: Path) extends VirtualDirectory {
    case class IRPath(ir: Path)

    def id(path: IRPath): String = {
      val fileName = path.ir.getFileName.toString.stripSuffix(".nir")
      val parent   = path.ir.getParent
      if (parent == null) fileName
      else parent.resolve(fileName).asScala.mkString(".")
    }

    private def resolve(path: Path): Path =
      this.path.resolve(path)

    private def open(path: Path) =
      FileChannel.open(
        path,
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        StandardOpenOption.TRUNCATE_EXISTING
      )

    override def read(path: IRPath)(implicit scope: Scope): ByteBuffer = {
      import java.nio.channels.FileChannel
      val channel = acquire(
        FileChannel.open(resolve(path.ir), StandardOpenOption.READ)
      )
      channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())
    }

    override def write(path: Path, buffer: ByteBuffer): Unit = {
      val channel = open(resolve(path))
      try channel.write(buffer)
      finally channel.close
    }

    override final val irFilePaths: Seq[IRPath] = {
      val allFilesIterator = Files
        .walk(path, Integer.MAX_VALUE, FileVisitOption.FOLLOW_LINKS)
        .iterator()
        .asScala
      allFilesIterator.flatMap { fp =>
        if (!fp.getFileName().toString.endsWith(".nir")) Nil
        else List(IRPath(path.relativize(fp)))
      }.toList
    }
  }

  private final class JarDirectory(path: Path)(implicit scope: Scope)
      extends VirtualDirectory {
    import java.util.zip.ZipEntry
    type IRPath = ZipEntry

    def id(path: IRPath): String = {
      @scala.annotation.tailrec
      def replace(
          b: StringBuilder,
          lastIndex: Int,
          before: String,
          after: String
      ): StringBuilder = {
        val index = b.indexOf(before, lastIndex)
        if (index == -1) b
        else {
          replace(b.replace(index, index + 1, after), index, before, after)
        }
      }

      val builder = new StringBuilder(path.getName)
      // Remove starting `/` and ending `.nir` from the path
      val length = builder.length
      val pruned = builder.delete(length - 4, length)
      replace(pruned, 0, "/", ".").result()
    }

    import org.zeroturnaround.zip.{ZipUtil, ZipInfoCallback}
    private val zipFile = path.toFile
    override def read(path: IRPath)(implicit scope: Scope): ByteBuffer = {
      val name = path.getName()
      ByteBuffer.wrap(
        ZipUtil.unpackEntry(zipFile, name, StandardCharsets.UTF_8)
      )
    }

    override def write(path: Path, buffer: ByteBuffer): Unit = {
      ???
    }

    override final val irFilePaths: Seq[IRPath] = {
      val nirFiles = mutable.UnrolledBuffer.empty[IRPath]
      ZipUtil.iterate(zipFile, new ZipInfoCallback() {
        def process(entry: ZipEntry): Unit = {
          if (entry.getName().endsWith(".nir")) {
            nirFiles += new IRPath(entry)
          }
        }
      })
      nirFiles
    }
  }

  private final object EmptyDirectory extends VirtualDirectory {
    final class IRPath
    override final val irFilePaths = Seq.empty

    def id(path: IRPath): String =
      throw new UnsupportedOperationException(
        "Can't obtain an id for IR path from an empty directory."
      )

    override def read(path: IRPath)(implicit scope: Scope): ByteBuffer =
      throw new UnsupportedOperationException(
        "Can't read from empty directory."
      )

    override def write(path: Path, buffer: ByteBuffer): Unit =
      throw new UnsupportedOperationException("Can't write to empty directory.")
  }
}
