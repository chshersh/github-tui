type t = {
  owner_repo : string;
  local_path : string option;
  log_file : string option;
  ignore_size_warning : bool;
  no_nerd_font : bool;
}

val init : t -> Model.initial_data
