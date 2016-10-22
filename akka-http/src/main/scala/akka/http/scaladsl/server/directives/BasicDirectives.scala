/*
 * Copyright (C) 2009-2016 Lightbend Inc. <http://www.lightbend.com>
 */

package akka.http.scaladsl.server
package directives

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.util.ByteString

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.collection.immutable
import akka.event.LoggingAdapter
import akka.http.scaladsl.model.Uri.Path
import akka.stream.impl.ConstantFun.scalaIdentityFunction
import akka.stream.{ ActorMaterializerHelper, Materializer }
import akka.http.scaladsl.settings.{ ParserSettings, RoutingSettings }
import akka.http.scaladsl.server.util.Tuple
import akka.http.scaladsl.util.FastFuture
import akka.http.scaladsl.model._
import akka.http.scaladsl.util.FastFuture._

import scala.util.{ Failure, Success }

/**
 * Basic directives
 *
 * $basicintrodoc
 *
 * @groupname basic-providers Basic directives - Providing Values to Inner Routes
 * @groupprio basic-providers 10
 * @groupdesc basic-providers Provide values to the inner routes with extractions.
 *
 * They can be distinguished on two axes:
 * a) provide a constant value or extract a value from the `RequestContext`
 * b) provide a single value or a tuple of values.
 *
 * $basicintrodoc
 *
 * @groupname basic-request-transformation Basic directives - Transforming the Request(Context)
 * @groupprio basic-request-transformation 11
 * @groupdesc basic-request-transformation
 * $basicintrodoc
 *
 * @groupname basic-response-transformation Basic directives - Transforming the Response
 * @groupprio basic-response-transformation 12
 * @groupdesc basic-response-transformation
 * Hook into the response path and transform the complete response or
 * the parts of a response or the list of rejections.
 *
 * $basicintrodoc
 *
 * @groupname basic-result-transformation Basic directives - Transforming the RouteResult
 * @groupprio basic-result-transformation 12
 * @groupdesc basic-result-transformation Transform the RouteResult of the inner route.
 *
 * $basicintrodoc
 *
 * @groupname basic-other Basic directives - Other
 * @groupprio basic-other 13
 * @groupdesc basic-other
 * $basicintrodoc
 *
 * @define basicintrodoc
 * These directives are building blocks for building
 * [[http://doc.akka.io/docs/akka-http/current/scala/http/routing-dsl/directives/custom-directives.html Custom Directives]]. As such they
 * usually aren't used in a route directly but rather in the definition of new
 * directives.
 */
trait BasicDirectives {

  /**
   * Changes the execution model of the inner route by wrapping it with arbitrary logic.
   *
   * The `mapInnerRoute` directive is used as a building block for [[http://doc.akka.io/docs/akka-http/current/scala/http/routing-dsl/directives/custom-directives.html Custom Directives]] to replace the inner route
   * with any other route. Usually, the returned route wraps the original one with custom execution logic.
   *
   * @example
   * {{{
   * val completeWithInnerException =
   *   mapInnerRoute { route => ctx =>
   *     try {
   *       route(ctx)
   *     } catch {
   *       case NonFatal(e) => ctx.complete(s"Got $${e.getClass.getSimpleName} '$${e.getMessage}'")
   *     }
   *   }
   *
   * val route =
   *   completeWithInnerException {
   *     complete(throw new IllegalArgumentException("BLIP! BLOP! Everything broke"))
   *   }
   *
   * // tests:
   * Get("/") ~> route ~> check {
   *   responseAs[String] shouldEqual "Got IllegalArgumentException 'BLIP! BLOP! Everything broke'"
   * }
   * }}}
   *
   * @group basic-other
   */
  def mapInnerRoute(f: Route ⇒ Route): Directive0 =
    Directive { inner ⇒ f(inner(())) }

  /**
   * @group basic-request-transformation
   */
  def mapRequestContext(f: RequestContext ⇒ RequestContext): Directive0 =
    mapInnerRoute { inner ⇒ ctx ⇒ inner(f(ctx)) }

  /**
   * @group basic-request-transformation
   */
  def mapRequest(f: HttpRequest ⇒ HttpRequest): Directive0 =
    mapRequestContext(_ mapRequest f)

  /**
   * @group basic-result-transformation
   */
  def mapRouteResultFuture(f: Future[RouteResult] ⇒ Future[RouteResult]): Directive0 =
    Directive { inner ⇒ ctx ⇒ f(inner(())(ctx)) }

  /**
   * @group basic-result-transformation
   */
  def mapRouteResult(f: RouteResult ⇒ RouteResult): Directive0 =
    Directive { inner ⇒ ctx ⇒ inner(())(ctx).fast.map(f)(ctx.executionContext) }

  /**
   * @group basic-result-transformation
   */
  def mapRouteResultWith(f: RouteResult ⇒ Future[RouteResult]): Directive0 =
    Directive { inner ⇒ ctx ⇒ inner(())(ctx).fast.flatMap(f)(ctx.executionContext) }

  /**
   * @group basic-result-transformation
   */
  def mapRouteResultPF(f: PartialFunction[RouteResult, RouteResult]): Directive0 =
    mapRouteResult(f.applyOrElse(_, identity[RouteResult]))

  /**
   * @group basic-result-transformation
   */
  def mapRouteResultWithPF(f: PartialFunction[RouteResult, Future[RouteResult]]): Directive0 =
    mapRouteResultWith(f.applyOrElse(_, FastFuture.successful[RouteResult]))

  /**
   * @group basic-result-transformation
   */
  def recoverRejections(f: immutable.Seq[Rejection] ⇒ RouteResult): Directive0 =
    mapRouteResultPF { case RouteResult.Rejected(rejections) ⇒ f(rejections) }

  /**
   * @group basic-result-transformation
   */
  def recoverRejectionsWith(f: immutable.Seq[Rejection] ⇒ Future[RouteResult]): Directive0 =
    mapRouteResultWithPF { case RouteResult.Rejected(rejections) ⇒ f(rejections) }

  /**
   * @group basic-result-transformation
   */
  def mapRejections(f: immutable.Seq[Rejection] ⇒ immutable.Seq[Rejection]): Directive0 =
    recoverRejections(rejections ⇒ RouteResult.Rejected(f(rejections)))

  /**
   * @group basic-response-transformation
   */
  def mapResponse(f: HttpResponse ⇒ HttpResponse): Directive0 =
    mapRouteResultPF { case RouteResult.Complete(response) ⇒ RouteResult.Complete(f(response)) }

  /**
   * @group basic-response-transformation
   */
  def mapResponseEntity(f: ResponseEntity ⇒ ResponseEntity): Directive0 =
    mapResponse(_ mapEntity f)

  /**
   * @group basic-response-transformation
   */
  def mapResponseHeaders(f: immutable.Seq[HttpHeader] ⇒ immutable.Seq[HttpHeader]): Directive0 =
    mapResponse(_ mapHeaders f)

  /**
   * A Directive0 that always passes the request on to its inner route
   * (i.e. does nothing with the request or the response).
   *
   * @group basic-other
   */
  def pass: Directive0 = Directive.Empty

  /**
   * Injects the given value into a directive.
   *
   * @group basic-providers
   */
  def provide[T](value: T): Directive1[T] = tprovide(Tuple1(value))

  /**
   * Injects the given values into a directive.
   *
   * @group basic-providers
   */
  def tprovide[L: Tuple](values: L): Directive[L] =
    Directive { _(values) }

  /**
   * Extract a single value from the [[RequestContext]] using the given function
   * and provide it to the inner route. It is a special case for extracting one
   * value of the more general [[textract]] directive that can be used to
   * extract more than one value.
   *
   * @example
   * {{{
   * val uriLength = extract(_.request.uri.toString.length)
   * val route =
   *   uriLength { len =>
   *     complete(s"The length of the request URI is $$len")
   *   }
   *
   * // tests:
   * Get("/abcdef") ~> route ~> check {
   *   responseAs[String] shouldEqual "The length of the request URI is 25"
   * }
   * }}}
   * @group basic-providers
   */
  def extract[T](f: RequestContext ⇒ T): Directive1[T] =
    textract(ctx ⇒ Tuple1(f(ctx)))

  /**
   * Extracts a number of values using the given function.
   *
   * @group basic-providers
   */
  def textract[L: Tuple](f: RequestContext ⇒ L): Directive[L] =
    Directive { inner ⇒ ctx ⇒ inner(f(ctx))(ctx) }

  /**
   * Adds a `TransformationRejection` cancelling all rejections equal to the
   * given one to the rejections potentially coming back from the inner route.
   *
   * Read [[http://doc.akka.io/docs/akka-http/current/scala/http/routing-dsl/rejections.html Rejections]] to learn more about rejections.
   *
   * For more advanced handling of rejections refer to the [[ExecutionDirectives.handleRejections]] directive
   * which provides a nicer DSL for building rejection handlers.
   *
   * @example
   * {{{
   * val route =
   *   cancelRejection(MethodRejection(HttpMethods.POST)) {
   *     post {
   *       complete("Result")
   *     }
   *   }
   *
   * // tests:
   * Get("/") ~> route ~> check {
   *   rejections shouldEqual Nil
   *   handled shouldEqual false
   * }
   * }}}
   *
   * @group basic-result-transformation
   */
  def cancelRejection(rejection: Rejection): Directive0 =
    cancelRejections(_ == rejection)

  /**
   * Adds a TransformationRejection cancelling all rejections for which the given
   * filter function returns true to the list of rejections potentially coming
   * back from the inner route.
   *
   * @see [[cancelRejections]]
   *
   * @group basic-result-transformation
   */
  def cancelRejections(classes: Class[_]*): Directive0 =
    cancelRejections(r ⇒ classes.exists(_ isInstance r))

  /**
   * Adds a `TransformationRejection` cancelling all rejections created by the inner route for which
   * the condition argument function returns `true`.
   *
   * Read [[http://doc.akka.io/docs/akka-http/current/scala/http/routing-dsl/rejections.html Rejections]] to learn more about rejections.
   *
   * For more advanced handling of rejections refer to the [[ExecutionDirectives.handleRejections]] directive
   * which provides a nicer DSL for building rejection handlers.
   *
   * @see [[cancelRejection]] for canceling a specific rejection.
   *
   * @example
   * {{{
   * def isMethodRejection: Rejection => Boolean = {
   *   case MethodRejection(_) => true
   *   case _                  => false
   * }
   *
   * val route =
   *   cancelRejections(isMethodRejection) {
   *     post {
   *       complete("Result")
   *     }
   *   }
   *
   * // tests:
   * Get("/") ~> route ~> check {
   *   rejections shouldEqual Nil
   *   handled shouldEqual false
   * }
   * }}}
   *
   * @group basic-result-transformation
   */
  def cancelRejections(cancelFilter: Rejection ⇒ Boolean): Directive0 =
    mapRejections(_ :+ TransformationRejection(_ filterNot cancelFilter))

  /**
   * Transforms the unmatchedPath of the RequestContext using the given function.
   *
   * @group basic-request-transformation
   */
  def mapUnmatchedPath(f: Uri.Path ⇒ Uri.Path): Directive0 =
    mapRequestContext(_ mapUnmatchedPath f)

  /**
   * Extracts the yet unmatched path from the RequestContext.
   *
   * @group basic-providers
   */
  def extractUnmatchedPath: Directive1[Uri.Path] = BasicDirectives._extractUnmatchedPath

  /**
   * Extracts the already matched path from the RequestContext.
   *
   * @group basic
   */
  def extractMatchedPath: Directive1[Uri.Path] = BasicDirectives._extractMatchedPath

  /**
   * Extracts the current [[HttpRequest]] instance.
   *
   * @group basic-providers
   */
  def extractRequest: Directive1[HttpRequest] = BasicDirectives._extractRequest

  /**
   * Extracts the complete request URI.
   *
   * @group basic-providers
   */
  def extractUri: Directive1[Uri] = BasicDirectives._extractUri

  /**
   * Runs its inner route with the given alternative [[scala.concurrent.ExecutionContextExecutor]].
   *
   * @group basic-request-transformation
   */
  def withExecutionContext(ec: ExecutionContextExecutor): Directive0 =
    mapRequestContext(_ withExecutionContext ec)

  /**
   * Extracts the [[scala.concurrent.ExecutionContextExecutor]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractExecutionContext: Directive1[ExecutionContextExecutor] = BasicDirectives._extractExecutionContext

  /**
   * Runs its inner route with the given alternative [[akka.stream.Materializer]].
   *
   * @group basic-request-transformation
   */
  def withMaterializer(materializer: Materializer): Directive0 =
    mapRequestContext(_ withMaterializer materializer)

  /**
   * Extracts the [[akka.stream.Materializer]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractMaterializer: Directive1[Materializer] = BasicDirectives._extractMaterializer

  /**
   * Extracts the [[akka.actor.ActorSystem]] from the [[RequestContext]] if the available Materializer is an [[akka.stream.ActorMaterializer]].
   * Otherwise throws an exception as it won't be able to extract the system from arbitrary materializers.
   *
   * Useful when the external API in your route needs one.
   *
   * @example
   * {{{
   * val route = extractActorSystem { actorSystem =>
   *   complete(s"Actor System extracted, hash=${actorSystem.hashCode()}")
   * }
   *
   * // tests:
   * Get("/") ~> route ~> check {
   *   responseAs[String] shouldEqual s"Actor System extracted, hash=${system.hashCode()}"
   * }
   * }}}
   * @group basic-providers
   */
  def extractActorSystem: Directive1[ActorSystem] = extract { ctx ⇒
    ActorMaterializerHelper.downcast(ctx.materializer).system
  }

  /**
   * Runs its inner route with the given alternative [[akka.event.LoggingAdapter]].
   *
   * @group basic-request-transformation
   */
  def withLog(log: LoggingAdapter): Directive0 =
    mapRequestContext(_ withLog log)

  /**
   * Extracts the [[akka.event.LoggingAdapter]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractLog: Directive1[LoggingAdapter] =
    BasicDirectives._extractLog

  /**
   * Runs its inner route with the given alternative [[RoutingSettings]].
   *
   * @group basic-request-transformation
   */
  def withSettings(settings: RoutingSettings): Directive0 =
    mapRequestContext(_ withRoutingSettings settings)

  /**
   * Runs the inner route with settings mapped by the given function.
   *
   * @group basic-request-transformation
   */
  def mapSettings(f: RoutingSettings ⇒ RoutingSettings): Directive0 =
    mapRequestContext(ctx ⇒ ctx.withRoutingSettings(f(ctx.settings)))

  /**
   * Extracts the [[RoutingSettings]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractSettings: Directive1[RoutingSettings] =
    BasicDirectives._extractSettings

  /**
   * Extracts the [[akka.http.scaladsl.settings.ParserSettings]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractParserSettings: Directive1[ParserSettings] =
    BasicDirectives._extractParserSettings

  /**
   * Extracts the [[akka.http.scaladsl.server.RequestContext]] itself.
   *
   * @group basic-providers
   */
  def extractRequestContext: Directive1[RequestContext] = BasicDirectives._extractRequestContext

  /**
   * Extracts the [[akka.http.scaladsl.model.RequestEntity]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractRequestEntity: Directive1[RequestEntity] = BasicDirectives._extractRequestEntity

  /**
   * Extracts the entities `dataBytes` [[akka.stream.scaladsl.Source]] from the [[akka.http.scaladsl.server.RequestContext]].
   *
   * @group basic-providers
   */
  def extractDataBytes: Directive1[Source[ByteString, Any]] = BasicDirectives._extractDataBytes

  /**
   * WARNING: This will read the entire request entity into memory regardless of size and effectively disable streaming.
   *
   * Converts the HttpEntity from the [[akka.http.scaladsl.server.RequestContext]] into an
   * [[akka.http.scaladsl.model.HttpEntity.Strict]] and extracts it, or fails the route if unable to drain the
   * entire request body within the timeout.
   *
   * @param timeout The directive is failed if the stream isn't completed after the given timeout.
   * @group basic-providers
   */
  def extractStrictEntity(timeout: FiniteDuration): Directive1[HttpEntity.Strict] =
    extract { ctx ⇒
      import ctx.materializer

      ctx.request.entity.toStrict(timeout)

    }.flatMap { entity ⇒
      import FutureDirectives._

      onComplete(entity).flatMap {
        case Success(x) ⇒ provide(x)
        case Failure(t) ⇒ StandardRoute(_.fail(t))
      }
    }

  /**
   * Transforms the request entity to strict entity before it is handled by the
   * inner route.
   *
   * A timeout parameter is given and if the stream isn't completed after the
   * timeout, the directive will be failed.
   *
   * @note (warning)
   * The directive will read the request entity into memory within the size
   * limit (8M by default) and effectively disable streaming.
   * The size limit can be configured globally with `akka.http.parsing.max-content-length` or
   * overridden by wrapping with [[MiscDirectives#withSizeLimit]] or
   * [[MiscDirectives#withoutSizeLimit]] directive.
   *
   * @example ../../../../../../../test/scala/docs/http/scaladsl/server/directives/BasicDirectivesExamplesSpec.scala#toStrictEntity-example
   *
   * @param timeout The directive is failed if the stream isn't completed after the given timeout.
   * @group basic-request-transformation
   */
  def toStrictEntity(timeout: FiniteDuration): Directive0 =
    Directive { inner ⇒ ctx ⇒
      import ctx.{ executionContext, materializer }

      ctx.request.entity.toStrict(timeout).flatMap { strictEntity ⇒
        val newCtx = ctx.mapRequest(_.copy(entity = strictEntity))
        inner(())(newCtx)
      }
    }

}

object BasicDirectives extends BasicDirectives {
  private val _extractUnmatchedPath: Directive1[Uri.Path] = extract(_.unmatchedPath)
  private val _extractMatchedPath: Directive1[Uri.Path] = extract(extractMatched)
  private val _extractRequest: Directive1[HttpRequest] = extract(_.request)
  private val _extractUri: Directive1[Uri] = extract(_.request.uri)
  private val _extractExecutionContext: Directive1[ExecutionContextExecutor] = extract(_.executionContext)
  private val _extractMaterializer: Directive1[Materializer] = extract(_.materializer)
  private val _extractLog: Directive1[LoggingAdapter] = extract(_.log)
  private val _extractSettings: Directive1[RoutingSettings] = extract(_.settings)
  private val _extractParserSettings: Directive1[ParserSettings] = extract(_.parserSettings)
  private val _extractRequestContext: Directive1[RequestContext] = extract(scalaIdentityFunction)
  private val _extractRequestEntity: Directive1[RequestEntity] = extract(_.request.entity)
  private val _extractDataBytes: Directive1[Source[ByteString, Any]] = extract(_.request.entity.dataBytes)

  private def extractMatched(ctx: RequestContext) = {
    val unmatchedPath = ctx.unmatchedPath.toString
    val fullPath = ctx.request.uri.path.toString

    require(
      fullPath.endsWith(unmatchedPath),
      s"Unmatched path '$unmatchedPath' wasn't a suffix of full path '$fullPath'. " +
        "This usually means that ctx.unmatchedPath was manipulated inconsistently " +
        "with ctx.request.uri.path"
    )

    Path(fullPath.substring(0, fullPath.length - unmatchedPath.length))
  }
}
