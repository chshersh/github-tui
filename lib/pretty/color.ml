type t = RGB of int * int * int

let pp fmt (RGB (r, g, b)) = Format.fprintf fmt "RGB(%d,%d,%d)" r g b
let show = Format.asprintf "%a" pp

let to_255 str =
  match int_of_string_opt ("0x" ^ str) with
  | None -> 0
  | Some c -> c

let rgb r g b = RGB (to_255 r, to_255 g, to_255 b)

let rgb str =
  match String.to_seq str |> List.of_seq |> List.map (String.make 1) with
  | [ "#"; r1; r2; g1; g2; b1; b2 ] -> rgb (r1 ^ r2) (g1 ^ g2) (b1 ^ b2)
  | _ -> RGB (0, 0, 0)

let of_hex str =
  if String.starts_with ~prefix:"#" str then rgb str else rgb ("#" ^ str)

(* Calculate the luminance of colour. When applied to background colour, this
   function could be used to detect the corresponding foreground colour. *)
let luminance (RGB (r, g, b)) =
  let scale x = float_of_int x /. 255.0 in
  let r, g, b = (scale r, scale g, scale b) in
  (0.2126 *. r) +. (0.7152 *. g) +. (0.0722 *. b)

let foreground color = if luminance color < 0.3 then `Light else `Dark
let to_escape_seq (RGB (r, g, b)) = Format.sprintf "2;%d;%d;%d" r g b
