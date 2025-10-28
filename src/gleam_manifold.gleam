import gleam/erlang/process
import gleam/erlang/reference

pub fn send(subject: process.Subject(message), message: message) -> Nil {
  let assert Ok(#(pid, ref)) = unwrap_subject(subject)
    as "Named subjects are currently unsupported in gleam_manifold.send()"
  manifold_send_subject(pid, #(ref, message))
  Nil
}

/// You almost certainly do not want to use this. 
/// Absolutely prefer to use the type-safe variant of
/// this function `manifold.send(subject, message)`
pub fn send_pid(pid: process.Pid, message: message) -> Nil {
  manifold_send_raw(pid, message)
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
pub fn send_multi_pid(pids: List(process.Pid), message: message) -> Nil {
  manifold_send_multi(pids, message)
  Nil
}

type DoNotLeak

@external(erlang, "gleam_manifold_ffi", "unwrap_subject")
fn unwrap_subject(
  subject: process.Subject(message),
) -> Result(#(process.Pid, reference.Reference), Nil)

@external(erlang, "Elixir.Manifold", "send")
fn manifold_send_subject(
  pid: process.Pid,
  message: #(reference.Reference, message),
) -> DoNotLeak

@external(erlang, "Elixir.Manifold", "send")
fn manifold_send_raw(pid: process.Pid, message: message) -> DoNotLeak

@external(erlang, "Elixir.Manifold", "send")
fn manifold_send_multi(pid: List(process.Pid), message: message) -> DoNotLeak
