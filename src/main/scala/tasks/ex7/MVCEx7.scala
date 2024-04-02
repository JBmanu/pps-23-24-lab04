package tasks.ex7

import tasks.ex7.CounterEx7Impl.*
import u03.extensionmethods.Streams.*
import u04.monads.*
import u04.monads.Monads.*
import u04.monads.Monads.Monad.{ seq, seqN }
import u04.monads.States.*
import u04.monads.WindowStateImpl.*

@main def MVCEx7 =
  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)


  def vmv[SM, SV, AM, AV](v1: State[SV, AM],
                          funConvertAM_SM: (AM, SM) => SM,
                          funChangeModel: (SM) => State[SM, AV],
                          funChangeView: (SM) => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (sv1, am) = v1.run(sv)
      val sm2 = funConvertAM_SM(am, sm)
      val (sm3, av) = funChangeModel(sm2).run(sm)
      val (sv2, av2) = funChangeView(sm2).run(sv1)
      ((sm3, sv2), av2)


  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(165, 300)
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
                           case "SetButton"   => vmv(textFieldText("TextField"), stringToCounter, set,
                                                     i => toLabel(i.toString, "Label1"))
                           case "QuitButton"  => mv(nop(), _ => exec(sys.exit()))))
  yield ()

  controller.run((initialCounter(), initialWindow))
