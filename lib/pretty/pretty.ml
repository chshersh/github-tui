module Color = Color
module Doc = Doc
module Layout = Layout

let render ~width ~height doc =
  doc |> Doc.render ~width ~height |> Layout.to_lines |> Extra.String.unlines
