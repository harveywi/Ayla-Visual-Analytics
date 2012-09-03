package edu.osu.compgeom.util

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

object IO {

  def withBufferedReader[T](f: File)(op: BufferedReader => T): T = get(new BufferedReader(new FileReader(f)), op)
  def withBufferedWriter[T](f: File)(op: BufferedWriter => T): T = put(new BufferedWriter(new FileWriter(f)), op)
  
  def withBufferedReader[T](is: InputStream)(op: BufferedReader => T): T = get(new BufferedReader(new InputStreamReader(is)), op)
  
  def withObjectInputStream[T](f: File)(op: ObjectInputStream => T): T = get(new ObjectInputStream(new FileInputStream(f)), op)
  def withObjectOutputStream[T](f: File)(op: ObjectOutputStream => T): T = put(new ObjectOutputStream(new FileOutputStream(f)), op)
  
  def withGzippedObjectInputStream[T](f: File)(op: ObjectInputStream => T): T = get(new ObjectInputStream(new GZIPInputStream(new FileInputStream(f))), op)
  def withGzippedObjectOutputStream[T](f: File)(op: ObjectOutputStream => T): T = put(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(f))), op)

  def get[A <: Closeable, B](source: A, op: A => B): B = {
    try {
      op(source)
    } finally {
      source.close
    }
  }

  def put[A <: Flushable with Closeable, B](dest: A, op: A => B): B = {
    try {
      op(dest)
    } finally {
      dest.flush
      dest.close
    }
  }

}