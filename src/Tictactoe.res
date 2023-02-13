open Belt
type action = Coords(int, int) | Restart

type board = array<array<int>>
type state = {
  board: array<array<int>>,
  move: int,
  winnerCoords: array<(int, int)>,
}

let init = () => {
  board: [[0, 0, 0], [0, 0, 0], [0, 0, 0]],
  move: 1,
  winnerCoords: [],
}

let state = init()

let toCheck = [
  [0, 1, 2],
  [3, 4, 5],
  [6, 7, 8],
  [0, 3, 6],
  [1, 4, 7],
  [2, 5, 8],
  [2, 4, 6],
  [0, 4, 8],
]

let coords = [
  [(0, 0), (0, 1), (0, 2)],
  [(1, 0), (1, 1), (1, 2)],
  [(2, 0), (2, 1), (2, 2)],
  [(0, 0), (1, 0), (2, 0)],
  [(0, 1), (1, 1), (2, 1)],
  [(0, 2), (1, 2), (2, 2)],
  [(0, 2), (1, 1), (2, 0)],
  [(0, 0), (1, 1), (2, 2)],
]

let freePos = (s: board) => {
  let fp = []
  s->Array.forEachWithIndex((i, x) => {
    x->Array.forEachWithIndex((j, y) => {
      if y == 0 {
        let _ = Js.Array.push((i, j), fp)
      }
    })
  })
  fp
}

let areSame = (r: array<int>) => {
  let a = r->Array.getUnsafe(0)
  let b = r->Array.getUnsafe(1)
  let c = r->Array.getUnsafe(2)
  a == b && b == c && a == c && a > 0
}

let check = (m: state) => {
  let s = m.board
  let row = []
  s->Array.forEachWithIndex((_, x) => {
    x->Array.forEachWithIndex((_, y) => {
      let _ = Js.Array.push(y, row)
    })
  })

  let a = toCheck->Array.map(x => {
    let e1 = x->Array.getUnsafe(0)
    let e2 = x->Array.getUnsafe(1)
    let e3 = x->Array.getUnsafe(2)
    let retVal = areSame([
      row->Array.getUnsafe(e1),
      row->Array.getUnsafe(e2),
      row->Array.getUnsafe(e3),
    ])
    retVal
  })

  let opt = a->Array.getIndexBy(x => x)
  let ind = opt->Option.getWithDefault(-1)
  let retVal = if ind >= 0 {
    coords[ind]
  } else {
    None
  }
  retVal
}

let nextCoords = (fp: array<(int, int)>) => {
  let l = Array.length(fp)
  if l == 0 {
    None
  } else {
    let r = Js.Math.random_int(0, l)
    fp->Array.get(r)
  }
}

let nextMove = (m: state) => {
  if m.move == 1 {
    2
  } else {
    1
  }
}

let isTerminal = (m: state): bool => {
  let s = m.board
  let b = s->Array.reduce(true, (x, y) => {
    let rv = y->Array.reduce(true, (p, q) => {
      p && q != 0
    })
    x && rv
  })
  m.winnerCoords->Array.length > 0 || b
}

let utility = (m: state): int => {
  if m.winnerCoords->Array.length == 0 {
    0
  } else {
    let (x, y) = m.winnerCoords->Array.getUnsafe(0)
    let row = m.board->Array.getUnsafe(x)
    let v = row->Array.getUnsafe(y)
    let retVal = if v == 1 {
      1
    } else {
      -1
    }
    retVal
  }
}

let freePosToState = (pos: (int, int), s: board, value: int): board => {
  let (p, q) = pos
  s->Array.mapWithIndex((i, x) => {
    x->Array.mapWithIndex((j, y) => {
      if i == p && q == j {
        value
      } else {
        y
      }
    })
  })
}

let rec minValue = (m: state): (int, option<(int, int)>) => {
  if isTerminal(m) {
    (utility(m), None)
  } else {
    let fp = freePos(m.board)
    let models = fp->Array.map(x => {
      let nm = nextMove(m)
      let s = freePosToState(x, m.board, nm)
      let newModel = {...m, board: s, move: nm}
      let opt = check(newModel)
      let sameCoords = opt->Option.getWithDefault([])
      let model = {board: s, move: nm, winnerCoords: sameCoords}
      let (value, _) = maxValue(model)
      (value, Some(x))
    })
    let acc = (1000, None)
    let (v, move) = models->Array.reduce(acc, (a, b) => {
      let (v1, move1) = a
      let (v2, move2) = b
      if v1 < v2 {
        (v1, move1)
      } else {
        (v2, move2)
      }
    })
    (v, move)
  }
}
and maxValue = (m: state): (int, option<(int, int)>) => {
  if isTerminal(m) {
    (utility(m), None)
  } else {
    let fp = freePos(m.board)
    let models = fp->Array.map(x => {
      let nm = nextMove(m)
      let s = freePosToState(x, m.board, nm)
      let newModel = {...m, board: s, move: nm}
      let opt = check(newModel)
      let sameCoords = opt->Option.getWithDefault([])
      let model = {board: s, move: nm, winnerCoords: sameCoords}
      let (value, _) = minValue(model)
      (value, Some(x))
    })
    let acc = (-1000, None)
    let (v, move) = models->Array.reduce(acc, (a, b) => {
      let (v1, move1) = a
      let (v2, move2) = b
      if v1 > v2 {
        (v1, move1)
      } else {
        (v2, move2)
      }
    })
    (v, move)
  }
}

let minimaxSearch = (m: state): (int, int) => {
  let (_, move) = maxValue(m)
  move->Option.getWithDefault((-1, -1))
}

let rec update = (model: state, msg: action) => {
  let winnerCoords = model.winnerCoords
  if winnerCoords->Array.length > 0 {
    switch msg {
    | Restart => init()
    | _ => model
    }
  } else {
    switch msg {
    | Restart => init()
    | Coords(i, j) => {
        let s = model.board->Array.mapWithIndex((n, x) =>
          x->Array.mapWithIndex((m, y) => {
            let p = nextMove(model)
            if n == i && m == j {
              p
            } else {
              y
            }
          })
        )
        let nm = nextMove(model)
        let newModel = {...model, board: s, move: nm}
        let opt = check(newModel)
        let sameCoords = opt->Option.getWithDefault([])

        //

        switch nm {
        | 1 => {...newModel, winnerCoords: sameCoords}
        | 2 =>
          let (x, y) = minimaxSearch(newModel)
          if (x, y) == (-1, -1) {
            {...newModel, winnerCoords: sameCoords}
          } else {
            let newModel = {...newModel, winnerCoords: sameCoords}
            update(newModel, Coords(x, y))
          }
        | _ => {...newModel, winnerCoords: sameCoords}
        }
      }
    }
  }
}

let cellStyle = (i: int, j: int, m: state, value: int) => {
  let wc = m.winnerCoords
  let style = if value == 2 {
    "cross"
  } else {
    "circle"
  }
  if Js.Array.findIndex(x => {x == (i, j)}, wc) >= 0 {
    `winner ${style}`
  } else {
    style
  }
}
