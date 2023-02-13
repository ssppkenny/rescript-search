%%raw("require('./styles.css')")
open Belt

let {getExn, flatMap} = module(Belt.Option)
let {document} = module(Webapi.Dom)
let {addEventListener, setInnerHTML} = module(Webapi.Dom.Element)
let {getElementById} = module(Webapi.Dom.Document)

type response
type product = {name: string, price: int}
type state = array<product>

@send external getJSON: response => Js.Promise.t<'a> = "json"

@react.component
let make = () => {
  let (searchText, setSearchText) = React.useState(_ => "")
  let (searchProducts, setSearchProducts) = React.useState(_ => [])
  let (count, setCount) = React.useState(_ => 0)

  React.useEffect1(() => {
    open Webapi.Fetch
    open Js.Promise

    let request = RequestInit.make(
      ~method_=Get,
      ~headers=HeadersInit.make({
        "Accept": "*/*",
        "Content-type": "application/json",
      }),
      (),
    )

    let _ =
      fetchWithInit(Js.String.concat(searchText, "/product?name="), request)
      ->then_(Response.json, _)
      ->then_(json => json->Js.Json.decodeArray->getExn->resolve, _)
      ->then_(array => {
        let t = Js.Array.map(
          x => {
            let obj = x->Js.Json.decodeObject->getExn
            {
              name: Js.Dict.get(obj, "name")->flatMap(Js.Json.decodeString)->getExn,
              price: Js.Dict.get(obj, "price")->flatMap(Js.Json.decodeNumber)->getExn->Float.toInt,
            }
          },
          array,
        )
        setSearchProducts(_ => t)
        resolve(t)
      }, _)
    Some(() => ())
  }, [count])

  let handleEvent = event => {
    let text = ReactEvent.Form.target(event)["value"]
    setSearchText(_ => text)
  }

  let elements = React.array(
    searchProducts->Array.map(x => {
      <div>
        <div className="product">
          <p> {x.name->React.string} </p>
        </div>
        <div className="price">
          <p> {x.price->Int.toString->React.string} </p>
        </div>
      </div>
    }),
  )

  let onClick = _evt => {
    setCount(prev => prev + 1)
  }

  <div id="container">
    <h2> {"Product Search"->React.string} </h2>
    <label htmlFor="product-text"> {"Enter search text:"->React.string} </label>
    <input id="product-text" type_="text" onChange={e => handleEvent(e)} />
    <button onClick> {"Search"->React.string} </button>
    {elements}
  </div>
}
