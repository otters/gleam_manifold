# gleam_manifold

Gleam bindings to [Manifold](https://github.com/discord/manifold) - an Elixir library for fast message passing between BEAM nodes.

## What is Manifold?

Manifold is an Elixir library developed by Discord that optimizes sending the same message to many processes. Instead of sending messages sequentially (which can be slow with thousands of processes), Manifold uses a divide-and-conquer approach that distributes the work across multiple sender processes, achieving much better performance at scale.

## Installation

Add to your `gleam.toml` as a git dependency:

```toml
[dependencies]
gleam_manifold = { git = "git@github.com:otters/gleam_manifold.git", ref = "<commit hash>" }
```

## Usage

### Creating and using Subjects

This library provides its own Subject type for type-safe message passing:

```gleam
import gleam/erlang/process
import gleam_manifold as manifold

pub fn example() {
  let subject = manifold.new_subject()
  let pid = process.self()

  // Send a message through Manifold
  manifold.send(pid, subject, "Hello world")

  // Receive the message
  let assert Ok(message) = manifold.receive(subject, 1000)
}
```

### Multicasting to multiple PIDs

Send the same message to multiple processes at once:

```gleam
import gleam/erlang/process
import gleam_manifold as manifold

pub fn broadcast(pids: List(process.Pid), message: String) {
  let subject = manifold.new_subject()
  manifold.send_multi(pids, subject, message)
}
```

## Current Limitations

**No options parameter support**: The Elixir Manifold library supports an options parameter for tuning performance characteristics (like partition size). This Gleam wrapper doesn't currently expose these options, though this could be added in the future.

## Testing

Run the tests with:

```bash
gleam test
```

## License

See the LICENSE file in the repository.
