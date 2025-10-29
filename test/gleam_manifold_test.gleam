import gleam/erlang/process
import gleam_manifold as manifold
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn simple_test() {
  let subject = manifold.new_subject()
  let pid = process.self()
  process.spawn(fn() { manifold.send(pid, subject, "Hello world") })
  assert manifold.receive(subject, 100) == Ok("Hello world")
}

pub fn many_pids_test() {
  let subject = manifold.new_subject()
  let pid = process.self()

  let proc = fn() {
    let selector = process.new_selector() |> process.select_other(identity)
    let assert Ok(#(_ref, result)) = process.selector_receive(selector, 5)
    manifold.send(pid, subject, result)
  }

  let a = process.spawn(proc)
  let b = process.spawn(proc)

  manifold.send(a, subject, "hello from a")
  manifold.send(b, subject, "hello from b")

  assert manifold.receive(subject, 5) == Ok("hello from a")
  assert manifold.receive(subject, 5) == Ok("hello from b")
  assert manifold.receive(subject, 5) == Error(Nil)
}

pub fn multi_pids_test() {
  let subject = manifold.new_subject()
  let pid = process.self()

  let proc = fn() {
    let selector = process.new_selector() |> process.select_other(identity)
    let assert Ok(#(_ref, result)) = process.selector_receive(selector, 5)
    manifold.send(pid, subject, result)
  }

  let a = process.spawn(proc)
  let b = process.spawn(proc)

  manifold.send_multi([a, b], subject, "hello from both")

  assert manifold.receive(subject, 5) == Ok("hello from both")
  assert manifold.receive(subject, 5) == Ok("hello from both")
  assert manifold.receive(subject, 5) == Error(Nil)
}

@external(erlang, "gleam_erlang_ffi", "identity")
fn identity(x: a) -> b
