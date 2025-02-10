module Doc = Doc
module Layout = Layout
module Style = Style
module Icon = Icon

let render ~width ~height doc =
  doc |> Doc.render ~width ~height |> Layout.to_lines |> Extra.String.unlines
