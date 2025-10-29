import gleam/erlang/process
import gleam/erlang/reference

pub opaque type Subject(message) {
  Subject(tag: reference.Reference)
}

type Message(message) =
  #(reference.Reference, message)

pub fn new_subject() -> Subject(message) {
  Subject(reference.new())
}

/// You almost certainly do not want to use this. 
/// Absolutely prefer to use the type-safe variant of
/// this function `manifold.send(subject, message)`
pub fn send(
  pid: process.Pid,
  subject: Subject(message),
  message: message,
) -> Nil {
  manifold_send(pid, #(subject.tag, message))
  Nil
}

/// Call Manifold.send with a list of pids as the first argument, as
/// is supported in Manifold
/// 
/// Sending multi does not support sending to subjects, because
/// subjects themselves EACH have a unique tag along with them. The
/// subject expects the tag to be included in the message itself, so
/// this would mean that all subjects need to have the same tag, which is
/// impossible because they are unique. You would have to use something
/// like `process.select_other` to take advantage of receiving values
/// with this function still within Gleam.
pub fn send_multi(
  pids: List(process.Pid),
  subject: Subject(message),
  message: message,
) -> Nil {
  manifold_send_multi(pids, #(subject.tag, message))
  Nil
}

type DoNotLeak

@external(erlang, "gleam_manifold_ffi", "receive")
pub fn receive(subject: Subject(message), timeout: Int) -> Result(message, Nil)

@external(erlang, "gleam_manifold_ffi", "receive")
pub fn receive_forever(subject: Subject(message)) -> message

@external(erlang, "Elixir.Manifold", "send")
fn manifold_send(pid: process.Pid, message: Message(message)) -> DoNotLeak

@external(erlang, "Elixir.Manifold", "send")
fn manifold_send_multi(
  pid: List(process.Pid),
  message: Message(message),
) -> DoNotLeak
