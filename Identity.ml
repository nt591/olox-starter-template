let id x = x

let%test _ = id 5 = 5
let%test _ = id "hello" = "hello"