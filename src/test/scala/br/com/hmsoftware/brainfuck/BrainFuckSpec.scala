package br.com.hmsoftware.brainfuck

import org.scalatest.concurrent.ScalaFutures
import org.scalatest._
import collection.mutable.ListBuffer

class BrainFuckSpec extends FreeSpec with Matchers with ScalaFutures {

  "BrainSpec" - {

    "Executa o incremento de um valor informado na entrada" in {

      val w = new BrainFuck
      w entrada Some( () => 13.toByte )
      var ss = ListBuffer[ Byte ]()
      w saida Some( s => { ss += s; println( s"###=====>>> $s" ) } )
      w depurar Some( s => println( s ) )
      w executar ",+."
      ss shouldEqual List( 14 ) }

    "Executa o somatório de zero a um valor infromado na entrada." in {

      var ss = ListBuffer[ Byte ]()
      val w = new BrainFuck entrada Some( 
        () => 13.toByte 
      ) saida Some( 
        s => { ss += s; println( s"###=====>>> $s" ) } 
      ) depurar Some( 
        s => println(s) 
      )
      w executar ",[->+<].>."
      ss shouldEqual List( 0, 13 ) }

    "Executa simplesmente 3" in {

      var ss = ListBuffer[ Byte ]()
      var d = List( 1, 2, 3, 4, 5 )
      val w = new BrainFuck
      w entrada Some( () => 
        if ( d.length > 0 ) { 
          val x = d.head.toByte; d = d.tail; x } 
        else 
          0.toByte )
      w saida Some( s => { ss += s; println( s"###=====>>> $s" ) } )
      w depurar Some( s => println( s ) )
      w executar ",[[->+<]>.<,]>."
      ss shouldEqual List( 1, 3, 6, 10 ) } } }

