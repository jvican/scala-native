package scala.scalanative.linker.benchmarks

import java.nio.file.{Path, Paths, Files}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import scala.scalanative.build

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode.SampleTime

import bloop.config.Config
import bloop.config.ConfigEncoderDecoders

@State(Scope.Benchmark)
abstract class LinkerBench extends scala.scalanative.linker.ReachabilitySuite {
  var bloopConfigPath: String = "/Users/jvican/Code/scala-native/.bloop/tests-test.json"

  private def parseConfig(jsonConfig: String): Config.File = {
    import io.circe.parser
    import ConfigEncoderDecoders._
    val parsed =
      parser.parse(jsonConfig).right.getOrElse(sys.error("error parsing"))
    allDecoder.decodeJson(parsed) match {
      case Right(parsedConfig) => parsedConfig
      case Left(failure)       => throw failure
    }
  }

  var classpath: List[Path] = _
  var workdir: Path = _

  @Setup(Level.Trial) def spawn(): Unit = {
    workdir = Files.createTempDirectory("linker-benchmarks")
    val configPath = Paths.get(bloopConfigPath)
    val configDir  = configPath.getParent
    val jsonContents =
      new String(Files.readAllBytes(configPath), StandardCharsets.UTF_8)
    val bloopConfig = parseConfig(jsonContents)

    val bloopArgs = List(
      "bloop",
      "compile",
      bloopConfig.project.name
    )

    import scala.collection.JavaConverters._
    val builder = new ProcessBuilder(bloopArgs.asJava)
    builder.directory(configDir.toFile)
    val bloopProcess = builder.start()
    bloopProcess.waitFor()

    classpath = bloopConfig.project.classesDir :: bloopConfig.project.classpath
  }

  @Benchmark
  def link(): Unit = {
    val config = build.Config.empty
      .withWorkdir(workdir)
      .withClassPath(classpath)
      // Hardcoded for the unit tests in the scala native build
      .withMainClass("scala.scalanative.testinterface.TestMain$")
      .withNativelib(classpath.find(_.toString.contains("nativelib")).head)
    val entries = build.ScalaNative.entries(config)
    val linked = build.ScalaNative.link(config, entries)
    assert(linked.unavailable.size == 0)
  }

  @TearDown(Level.Trial) def terminate(): Unit = {
  }
}

@Fork(1)
@State(Scope.Benchmark)
@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 30, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 500, timeUnit = TimeUnit.MILLISECONDS)
class HotLinkerBench extends LinkerBench