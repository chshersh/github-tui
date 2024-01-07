let view (model: Model.t) =
  match model.tab with
  | Code -> Format.sprintf
    {|
Code

|}
  | Issues -> Format.sprintf
    {|
Issues

|}
  | PullRequests -> Format.sprintf 
    {|
Pull Requests

|}