import sbt._

/** Fetches a pinned archive of prebuilt ROMs into .rom-cache. Lives in project/ because sbt does not hoist objects out of a build.sbt. */
object RomSources {

  def fetch(log: Logger, base: File, name: String, url: String): File = {
    val cache = base / ".rom-cache"
    val archive = cache / s"$name.zip"
    val unpacked = cache / name

    // Recursive, since Mooneye's ROMs sit three levels down.
    if ((unpacked ** "*.gb").get.isEmpty) {
      IO.createDirectory(cache)

      if (!archive.exists()) {
        log.info(s"Fetching $name from $url")
        val connection = java.net.URI.create(url).toURL.openConnection()
        connection.setRequestProperty("User-Agent", "s4gb")
        val in = connection.getInputStream
        try java.nio.file.Files.copy(in, archive.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        finally in.close()
      }

      IO.delete(unpacked)
      IO.createDirectory(unpacked)
      IO.unzip(archive, unpacked)
    }

    // Resolved only once the content exists. Mealybug's archive puts the ROMs at the
    // root of the zip, Mooneye's nests everything under a single build-named directory.
    val nested = (unpacked * "*").get.filter(_.isDirectory).toList
    val roms = if ((unpacked * "*.gb").get.isEmpty && nested.length == 1) nested.head else unpacked

    log.info(s"$name: ${(roms ** "*.gb").get.size} ROMs in $roms")
    roms
  }
}