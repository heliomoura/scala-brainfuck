package br.com.hmsoftware.brainfuck

/** Interpretador da linguagem BrainFuck.
  *
  * Comandos da linguagem BrainFuck:
  *
  *   ,  Lê um byte e o coloca na posição corrente da memória.
  *   .  Imprime o byte da posição corrente da memória.
  *   >  Avança uma posição na memória.
  *   <  Recua uma posição na memória.
  *   +  Incrementa a posição corrente da memória de 1.
  *   -  Decrementa a posição corrente da memória de 1.
  *   [  Se a posição corrente da memória for diferente de zero vai para o próximo comando
  *      senão vai para o comando seguinte ao ']' casado com este '['.
  *   ]  Se a posição corrente da memória for igual a zero vai para o próximo comando
  *      senão vai para o comando seguinte ao '[' casado com este ']'.
  *
  *   Ignora qualquer outro caractere, inclusive quebra de linhas, permitindo a inclusao de comentarios a vontade.
  *
  * Helio Moura - helioasmoura@gmail.com */
class BrainFuck {

  /** Função para ler um byte da entrada. */
  private var ent: Option[ () => Byte ] = None

  /** Função para escrever um byte na saída. */
  private var sai: Option[ Byte => Unit ] = None

  /** Função para imprimir um texto de depuração. */
  private var dep: Option[ String => Unit ] = None

  /** memória (cresce conforme seu uso). */
  private val mem = collection.mutable.ArrayBuffer[ Byte ]()

  /** Posicao corrente da memória. */
  private var kmem = 0

  /** Código executável compilado. */
  private var exe = Array[ Char ]()

  /** Posicao corrente do código executável. */
  private var kexe = 0

  /** Pilha de laços ativos.
    * Seu topo contém a posição inicial do laço corrente. */
  private val pLacos = collection.mutable.Stack[ Int ]()

  /** Contador de passos do processador. */
  private var passo = 0

  /** Definição da função de entrada.
    * @param f Função de entrada a ser utilizada.
    * @return Este próprio objeto, permitindo chamada encadeada. */
  def entrada( f: Option[ () => Byte ] ) = { ent = f; this }

  /** Definição da função de saída.
    * @param f Função de saída a ser utilizada.
    * @return Este próprio objeto, permitindo chamada encadeada. */
  def saida( f: Option[ Byte => Unit ] ) = { sai = f; this }

  /** Definição da função de depuração.
    * @param f Função de depuração a ser utilizada.
    * @return Este próprio objeto, permitindo chamada encadeada. */
  def depurar( f: Option[ String => Unit ] ) = { dep = f; this }

  /** Comanda a execução de um programa BrainFuck.
    * @param programa Programa BrainFuck a ser executado.
    * @return Este proprio objeto, permitindo chamada encadeada. */
  def executar( programa: String ) = {
    ent match {
      case Some( e ) =>
        sai match {
          case Some( s ) => compilar( programa ); zerar; rodar( e, s )
          case None => throw new Exception( "Saida nao foi definida." ) }
      case None => throw new Exception("Entrada nao foi definida.") }
    this }

  /** Compila um programa BrainFuck retirando comentários e movendo o código para um vetor.
    * @param prg Programa BrainFuck a ser executado. */
  private def compilar( prg: String ) = exe = ( prg filter ( "[]+-<>,." contains _ ) ).toArray

  /** Reinicia o processador de BrainFuck. */
  private def zerar = { mem.clear; mem += 0; kmem = 0; pLacos.clear; kexe = 0; passo = 0 }

  /** Busca a saída do laço corrente. */
  private def saiLaco( n: Int = 0 ): Unit =
    if ( exe( kexe ) != ']' )
      if ( exe( kexe ) == '[' ) { kexe += 1; saiLaco( n + 1 ) }
      else { kexe += 1; saiLaco( n ) }
    else
      if ( n > 0 ) { kexe += 1; saiLaco( n - 1 ) }
      else { pLacos.pop; kexe += 1 }

  /** Interpretador de BrainFuck.
    * @param e Função de Entrada.
    * @param s Função de Saida. */
  private def rodar(e: () => Byte, s: Byte => Unit): Unit = {
    passo += 1
    if ( passo < 100 && kexe < exe.length ) {
      dep match {
        case Some(d) => d( s"$passo# p($kexe):${ exe( kexe ) } m($kmem):${ mem( kmem ) }" )
        case None => }
      exe(kexe) match {
        case '[' =>
          if ( mem( kmem ) != 0 ) { kexe += 1; pLacos push kexe }
          else saiLaco()
        case ']' =>
          if ( mem( kmem ) != 0 ) kexe = pLacos.top
          else { kexe += 1; pLacos.pop }
        case '+' =>
          mem( kmem ) = ( mem( kmem ) + 1 ).toByte
          kexe += 1
        case '-' =>
          mem( kmem ) = ( mem( kmem ) - 1 ).toByte
          kexe += 1
        case '<' =>
          if ( kmem > 0 ) kmem -= 1
          kexe += 1
        case '>' =>
          kmem += 1
          if ( kmem == mem.length ) mem += 0
          kexe += 1
        case '.' =>
          s( mem( kmem ) ); kexe += 1
        case ',' =>
          mem( kmem ) = e(); kexe += 1 }
      rodar( e, s ) } } }

