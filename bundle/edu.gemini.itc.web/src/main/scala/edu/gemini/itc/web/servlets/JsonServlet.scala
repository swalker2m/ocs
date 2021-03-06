package edu.gemini.itc.web.servlets

import argonaut._, Argonaut._
import edu.gemini.itc.shared.{ ItcParameters, ItcResult, ItcService }
import edu.gemini.itc.service.ItcServiceImpl
import edu.gemini.itc.web.json.{ ItcParametersCodec, ItcResultCodec }
import javax.servlet.http.{ HttpServlet, HttpServletRequest, HttpServletResponse }
import javax.servlet.http.HttpServletResponse.{ SC_BAD_REQUEST, SC_OK }
import scala.io.Source
import scalaz._, Scalaz._

/**
 * Servlet that accepts a JSON-encoded `ItcParameters` as its POST payload (no other methods are
 * supported) and responds with a JSON-encoded `ItcResult` on success, or `SC_BAD_REQUEST` with
 * an error message on failure. JSON codecs are defined in package `edu.gemini.itc.web.json`.
 */
class JsonServlet extends HttpServlet with ItcParametersCodec with ItcResultCodec {

  override def doPost(req: HttpServletRequest, res: HttpServletResponse) = {

    // I don't know if this is threadsafe so we'll forge one per-request.
    val itc: ItcService = new ItcServiceImpl

    // Read the body, which with some luck is a JSON string
    val enc  = Option(req.getCharacterEncoding).getOrElse("UTF-8")
    val src  = Source.fromInputStream(req.getInputStream, enc)
    val json = try src.mkString finally src.close

    // Do the things.
    val result: Either[String, ItcResult] =
      for {
        itcReq <- Parse.decodeEither[ItcParameters](json)
        itcRes <- itc.calculate(itcReq, true).toEither.leftMap(_.msg)
      } yield itcRes

    // Send our result back.
    result match {
      case Left(err)     => res.sendError(SC_BAD_REQUEST, err)
      case Right(itcRes) =>
        res.setStatus(SC_OK)
        res.setContentType("text/json; charset=UTF-8")
        val writer = res.getWriter // can only be called once :-\
        writer.write(itcRes.asJson.spaces2)
        writer.close
    }

  }

}
