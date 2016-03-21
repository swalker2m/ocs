package edu.gemini.spdb.rapidtoo.www

import edu.gemini.shared.util.immutable.ScalaConverters._
import edu.gemini.spModel.core.Magnitude

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Prop.forAll

object MagParserSpec extends Specification with ScalaCheck with edu.gemini.spModel.core.Arbitraries {


  "MagParser" should {

    "Parse Valid Mags" ! forAll { (ms: List[Magnitude]) =>
      val s = ms.map(m => s"${m.value}/${m.band}/${m.system}").mkString(",")
      new MagParser().unsafeParse(s) must_== ms.asImList
    }

    "Fail on Invalid Mags" in {
      new MagParser().unsafeParse("banana") must throwA[BadRequestException]
    }

  }

}
