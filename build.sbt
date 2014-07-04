name := "SBTModel"

scalaVersion := "2.10.1"

version := "1.0"

unmanagedBase <<= baseDirectory { base => base / "lib" }

