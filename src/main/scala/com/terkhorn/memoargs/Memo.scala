package com.terkhorn.memoargs


import java.util.UUID

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.meta._
import scala.meta.contrib._

class Memo(millis: Long) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case defn: Defn.Def =>

        /** @see https://stackoverflow.com/questions/41664590/passing-type-parameter-to-scala-meta-macro-annotations
          */
        val ttl = this match {
          // The argument needs to be a literal like `1`
          case q"new $_(${Lit(arg: Int)})" => arg.toLong
          case q"new $_(${Lit(arg: Long)})" => arg
          case _ => 10L // default value
        }

        val innerDefn = defn.copy(name = Term.Name(Memo.addInnerSuffix(defn.name.value)))

        val argss = Memo.toArgss(defn.paramss)

        // i don't really understand how this works, but it does
        val namesToValues: Seq[Term.Tuple] = defn.paramss.flatten.map { param =>
          q"(${param.name.syntax}, ${Term.Name(param.name.value)})"
        }
        val namesToValuesTerm: Term =
          q"_root_.scala.collection.Map[String, Any](..$namesToValues)"

        val methodId = UUID.randomUUID().toString
        val updatedBody =
          q"""
              {
                val key = ($methodId,$namesToValuesTerm)

                $innerDefn

                _root_.com.terkhorn.memoargs.Memo.cache.get(key) match {
                  case Some((r, expiry)) if _root_.java.lang.System.currentTimeMillis() < expiry =>
                    r

                  case _ =>
                    val result = ${innerDefn.name}(...$argss)
                    _root_.com.terkhorn.memoargs.Memo.cache(key) = (result, _root_.java.lang.System.currentTimeMillis() + $ttl)
                    result

                }
              }
           """
        defn.copy(body = updatedBody)
      case other => abort(other.pos, "@Memo must annotate a method.")
    }
  }

}

object Memo {

  private val innerSuffix = "$Inner"

  private[memoargs] def addInnerSuffix(name: String): String = name + innerSuffix

  private[memoargs] def toArgss(params: Seq[Seq[Term.Param]]): Seq[Seq[Term.Name]] = params.map(_.map(param => param.name.asTerm))

  /** Map of (fully qualified method name, argument) -> (value, expiry) */
  val cache: mutable.Map[(String, Any), (Any, Long)] = mutable.Map.empty

}
