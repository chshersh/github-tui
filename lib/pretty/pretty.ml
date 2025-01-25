module Color = Color
module Doc = Doc
module Layout = Layout

let render ~width doc =
  doc |> Doc.render ~width |> Layout.to_lines |> Extra.String.unlines
