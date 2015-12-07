node test {
  user { 'jenkins':
    groups => "docker"
  }
  User <| title == 'jenkins' |> { groups +> "dockerroot" }
}
