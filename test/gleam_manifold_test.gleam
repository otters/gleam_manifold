import gleam/erlang/process
import gleam_manifold as manifold
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn simple_test() {
  let subject = process.new_subject()
  process.spawn(fn() { manifold.send(subject, "Hello world") })
  assert process.receive(subject, 100) == Ok("Hello world")
}

pub fn many_pids_test() {
  let subject = process.new_subject()

  let proc = fn() {
    let selector = process.new_selector() |> process.select_other(identity)
    let assert Ok(result) = process.selector_receive(selector, 5)
    process.send(subject, result)
  }

  let a = process.spawn(proc)
  let b = process.spawn(proc)

  manifold.send_pid(a, "hello from a")
  manifold.send_pid(b, "hello from b")

  assert process.receive(subject, 5) == Ok("hello from a")
  assert process.receive(subject, 5) == Ok("hello from b")
  assert process.receive(subject, 5) == Error(Nil)
}

pub fn multi_pids_test() {
  let subject = process.new_subject()

  let proc = fn() {
    let selector = process.new_selector() |> process.select_other(identity)
    let assert Ok(result) = process.selector_receive(selector, 5)
    process.send(subject, result)
  }

  let a = process.spawn(proc)
  let b = process.spawn(proc)

  manifold.send_multi_pid([a, b], "hello from both")

  assert process.receive(subject, 5) == Ok("hello from both")
  assert process.receive(subject, 5) == Ok("hello from both")
  assert process.receive(subject, 5) == Error(Nil)
}

@external(erlang, "gleam_erlang_ffi", "identity")
fn identity(x: a) -> b
