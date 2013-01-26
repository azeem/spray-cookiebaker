package com.example.sessionutils

import shapeless._
import spray.routing._
import directives.BasicDirectives._
import spray.routing.directives._
import CookieDirectives._
import scala.Some
import shapeless.::

/**
 * Created with IntelliJ IDEA.
 * User: azeem
 * Date: 1/20/13
 * Time: 5:52 PM
 * To change this template use File | Settings | File Templates.
 */
object SessionDirectives {
  def bakedCookie[T <: AnyRef](baker: CookieBaker[T]):Directive[T :: HNil] = cookie(baker.cookieName).hmap {
    case c :: HNil => baker.decodeFromCookie(Some(c))
  }
  def optionalBakedCookie[T <: AnyRef](baker: CookieBaker[T]):Directive[Option[T] :: HNil] =
      bakedCookie(baker).hmap(_.map(shapeless.option)) | provide(None)

  def setBakedCookie[T <: AnyRef](baker: CookieBaker[T], data: T):Directive0 = setCookie(baker.encodeAsCookie(data))
  def clearBakedCookie[T <: AnyRef](baker: CookieBaker[T]):Directive0 = setCookie(baker.discard)

  def session:Directive[Session :: HNil] = bakedCookie(Session)
  def optionalSession:Directive[Option[Session] :: HNil] = optionalBakedCookie(Session)
  def setSession(sessionData: Session):Directive0 = setBakedCookie(Session, sessionData)
  def setSession(mapData: (String, String)*):Directive0 = setSession(Session(Map(mapData:_*)))
  def clearSession:Directive0 = clearBakedCookie(Session)
}
