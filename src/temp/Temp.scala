package temp

import java.io._
import edu.osu.compgeom.util.IO._
import edu.osu.compgeom.ayla.AylaCollaborationProject
import org.netlib.blas.BLAS
import edu.osu.compgeom.util.Timing

object Temp {

  @inline
  final def euclidDistSq(p1: Array[Float], p2: Array[Float]): Double = {
    var sum = 0d
    var i = 0
    val n = p1.length
    while (i < n) {
      val diff = p1(i) - p2(i)
      sum += diff * diff
      i += 1
    }
    sum
  }

  def main(args: Array[String]): Unit = {
//    println("Hello")
//    System.out.println(BLAS.getInstance().getClass().getName())
//
//    val v1 = Array.fill(25299600)(math.random.toFloat)
//    val v2 = Array.fill(25299600)(math.random.toFloat)
//    
//    val naive = Timing("Naive") {
//      math.sqrt(euclidDistSq(v1, v2))
//    }
//    
//    val blas = BLAS.getInstance()
//    val vDist = new Array[Float](v1.length)
//    
//    val result = Timing("BLAS") {
//      blas.scopy(v1.length,v1,1,vDist,1)
//      blas.saxpy(v1.length,-1f,v2,1,vDist,1)
//      blas.snrm2(v1.length,vDist,1)
//    }
//
////    val result = BLAS.getInstance().sdot(v1.size, v1, 1, v1, 1)
//
//    println("Naive:  %f\tBLAS:  %f".format(naive, result))
//
//    System.exit(0)

    //    val f = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/pcd_v2.dat")
    //    val pts = withObjectInputStream(f){ois => 
    //      ois.readObject.asInstanceOf[Array[Array[Float]]]
    //    }
    //    withBufferedWriter(new File("/dev/shm/out.csv")){bw =>
    //      pts.foreach(p => bw.write(p.take(100).mkString("", ",", "\n")))
    //    }
    //    println("ok")

    val f = new File("/home/harveywi/research/ScalaCrystals/Survivin_F93_F101/collab_projects")
    //    val oldProjNames = List("20000_k15_low_e.dat")
    f.listFiles.foreach { oldProj =>
      val oldProjName = oldProj.getName
      withObjectInputStream(oldProj) { ois =>
        val oldProj = ois.readObject.asInstanceOf[AylaCollaborationProject]
        val newProj = new AylaCollaborationProject(new File("/media/twogig/ayla/Survivin_F93_F101/collab_projects/" + oldProjName), oldProj.sf, oldProj.sampledToUnsampled)
        newProj.name = oldProj.name
        newProj.description = oldProj.description
        newProj.dateCreated = oldProj.dateCreated
        newProj.dateModified = oldProj.dateModified
        withObjectOutputStream(new File("/home/harveywi/Desktop/" + oldProjName)) { _.writeObject(newProj) }
      }
    }

    //    val oldPath = "/media/My Passport/survivin/"
    //    val newPath = "/media/twogig/survivin/"
    //    withBufferedWriter(new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/conformation_filenames_thor.txt")) { bw =>
    //      withBufferedReader(f) { br =>
    //        while (br.ready) {
    //          val line = br.readLine()
    //          val newAbsPath = line.replaceFirst(oldPath, newPath)
    //          bw.write(newAbsPath + "\n")
    //        }
    //      }
    //    }

    //    val f = new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin/pca3D_v2.dat")
    //    val pca3d = withObjectInputStream(f){(_.readObject().asInstanceOf[Array[Array[Float]]])}
    //    val pcaDropZeroes = pca3d.map(p => p.take(3))
    //    withObjectOutputStream(f){_.writeObject(pcaDropZeroes)}
    //    println("Done.")

    //    val dir = new File("/home/harveywi/research/ScalaCrystals/Survivin_20000/scalar_functions/SASA")
    //    assert(dir.exists)
    //    
    //    dir.listFiles.filter(_.getName.endsWith(".txt")).foreach{file =>
    //      val funcVals = withBufferedReader(file){br =>
    //        // Drop first line
    //        br.readLine
    //        Iterator.continually(br.readLine).takeWhile(_ != null).map(_.toInt).toArray
    //      }
    //      
    //      val residueID = file.getName.takeWhile(_ != '.').toInt
    //      
    //      withBufferedWriter(file){bw =>
    //        bw.write("SASA " + residueID + "\n")
    //        funcVals.foreach(f => bw.write(f + "\n"))
    //      }
    //    }
    //    println("Done!")
  }

}