let t _ = true

let%test _ = t false = true
let%test _ = t 1234 = true