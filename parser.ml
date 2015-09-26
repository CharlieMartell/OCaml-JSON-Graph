open Core.Std
open Yojson.Basic.Util;;

(* Read the JSON file *)
let map_data_json = Yojson.Basic.from_file "map_data.json";;

(* Define data types for JSON mappings *)
(* Data sources define where data comes from *)
let data_sources = map_data_json |> member "data sources" |> to_list |> filter_string;;

(* Define coord and metro type for parsing metros *)
type coord = {
  north         : string;
  south         : string;
  east          : string;
  west          : string;
}

type metros = {
  code          : string;
  name          : string;
  country       : string;
  continent     : string;
  (*timezone      : int;*)
  (*coordinates   : coord;*)
  population    : int;
  region        : int;
};;


let metro_list = map_data_json |> member "metros" |> to_list;;
let metro_test = List.map metro_list 
    ~f:(fun map_data_json -> { 
      code          = member "code" map_data_json |> to_string;
      name          = member "name" map_data_json |> to_string;
      country       = member "country" map_data_json |> to_string;
      continent     = member "continent" map_data_json |> to_string;
      (*timezone      = member "timezone" map_data_json |> to_int;*)
      population    = member "population" map_data_json |> to_int;
      region        = member "region" map_data_json |> to_int;
});;
let metro_names = List.map metro_list ~f:(fun map_data_json -> member "name" map_data_json |> to_string);;
(* Print the results of the parsing *)
printf "data sources: %s\n" (String.concat ~sep:", " data_sources);

(*
printf "metro test: %s\n" (String.concat ~sep:", " ~f:(fun metro_test -> metro));
printf "metro test: code: %s, name: %s, country: %s, continent: %s, timezone: %d, population: %d, region: %d\n" 
metro.code, metro.name, metro.country, metro.continent, metro.timezone, metro.population, metro.region in List.iter f metro_test;;
*)
printf "metro names: %s\n" (String.concat ~sep:", " metro_names);
