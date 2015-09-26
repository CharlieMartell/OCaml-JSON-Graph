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
  coordinates   : coord;
  population    : int;
  region        : int;
};;


let metro_list = map_data_json |> member "metros" |> to_list;;
let metro_test = List.map metro_list 
    ~f:(fun x -> { 
      code          = member "code" x |> to_string;
      name          = member "name" x |> to_string;
      country       = member "country" x |> to_string;
      continent     = member "continent" x |> to_string;
      (*timezone      = member "timezone" x |> to_int;*)
      (*coordinates   = member "coordinates" x |> to_int;*)
      population    = member "population" x |> to_int;
      region        = member "region" x |> to_int;
});;

let metro_codes = List.map metro_test ~f:(fun x -> x.code);;
let metro_names = List.map metro_test ~f:(fun x -> x.name);;
let metro_countries = List.map metro_test ~f:(fun x -> x.country);;
let metro_continents = List.map metro_test ~f:(fun x -> x.continent);;
let metro_populations = List.map metro_test ~f:(fun x -> string_of_int x.population);;
let metro_regions = List.map metro_test ~f:(fun x -> string_of_int x.region);;

(* Print the results of the parsing *)
(* Print data sources *)
printf "data sources: %s\n" (String.concat ~sep:", " data_sources);
(* Print metro data *)
printf "metro codes: %s\n" (String.concat ~sep:", " metro_codes);
printf "metro names: %s\n" (String.concat ~sep:", " metro_names);
printf "metro countries: %s\n" (String.concat ~sep:", " metro_countries);
printf "metro continents: %s\n" (String.concat ~sep:", " metro_continents);
printf "metro population: %s\n" (String.concat ~sep:", " metro_populations);
printf "metro region: %s\n" (String.concat ~sep:", " metro_regions);
