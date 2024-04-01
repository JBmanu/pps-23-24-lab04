package tasks.ex7

import tasks.ex7.CounterEx7Impl.*
import u03.extensionmethods.Streams.*
import u04.monads.*
import u04.monads.CounterStateImpl.counter
import u04.monads.Monads.*
import u04.monads.Monads.Monad.{ seq, seqN }
import u04.monads.States.*
import u04.monads.WindowStateImpl.*

@main def MVCEx7 =
  // m1 modello iniziale a quello finale
  // f preso un modello finale => la view iniziale e finale
  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      println("MODEL START: " + sm)
      println("VIEW START: " + sv)
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)
  
  
  def vmv[SM, SV, AM, AV](v1: State[SV, AM], fm: (AM) => State[SM, AV], fv: (SM) => State[SV, AV]): State[(SM, SV), AV] = 
    State: (sm, sv) =>
      println("MODEL START: " + sm)
      println("VIEW START: " + sv)
      val (sv1, am) = v1.run(sv)
      val (sm2, av) = fm(am).run(sm)
      val (sv2, av2) = fv(sm2).run(sv1)
      println("MODEL FINALE: " + av)
      println("VIEW FINALE: " + sv2)
      ((sm2, sv2), av)

  def vm[SM, SV, AM, AV](v1: State[SV, AM], f: (AM) => State[SV, AV]): State[(SM, SV), AV] =
    // sm => stato modello iniziale
    // sv => stato view
    State: (sm, sv) =>
      println("MODEL START: " + sm)
      println("VIEW START: " + sv)
      // runno lo stato dalla view iniziale => State(view(iniziale), model(finale))
      val (sv1, am) = v1.run(sv)
      println("MODEL FINALE: " + am)
      println("VIEW FINALE: " + sv1)
      // runno f stato final => State(view(finale), modello(finale)) 
      val (sv2, av) = f(am).run(sv1)
      println("MODEL FINALE: " + av)
      println("VIEW FINALE: " + sv2)
      ((sm, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addButton(text = "set", name = "SetButton")
    _ <- addTextField(text = str, name = "TextField")
    _ <- addLabel(text = str, name = "Label1")
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(seq(reset(), get()), i => windowCreation(i.toString()))
    _ <- seqN(events.map(_ match
                           case "IncButton"   => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
                           case "DecButton"   => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
                           case "ResetButton" => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
                           case "SetButton"   => vmv(textFieldText("TextField"), 
                                                     i => set(counter(i.toInt)), i => toLabel(i.toString, "Label1"))
                           case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
  yield ()


  controller.run((initialCounter(), initialWindow))
